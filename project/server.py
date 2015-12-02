import time
import logging
import sys
import json

from twisted.internet import reactor, protocol
from twisted.protocols.basic import LineReceiver
from twisted.python import log
from twisted.web.client import getPage
from twisted.application import service, internet

# Global Google API setup
GOOGLE_PLACE_API_KEY = "AIzaSyDfbd9R06aZ304FMrqYD34sMrVKMrepv6E"
GOOGLE_PLACE_API_PREFIX = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"

# Global setup; this is expected to be runned from a general script/controller, 
#  that knows the topology and IP/port mapping
servers = {
	"Alford" :   {"ip":"localhost", "port": 12490},
	"Bolden" :   {"ip":"localhost", "port": 12491},
	"Hamilton" : {"ip":"localhost", "port": 12492},
	"Parker" :   {"ip":"localhost", "port": 12493},
	"Powell" :   {"ip":"localhost", "port": 12494}
}

neighbors = {
	"Alford" :   ["Parker", "Powell"],
	"Bolden" :   ["Parker", "Powell"],
	"Hamilton" : ["Parker"],
	"Parker" :   ["Alford", "Bolden", "Hamilton"],
	"Powell" :   ["Alford", "Bolden"]
}

class ProxyServerProtocol(LineReceiver):
  def __init__(self, factory):
    self.factory = factory

  def connectionMade(self):
    # connectionNum is the total number of connection, including client-this server and server(as client)-this server
    self.factory.connectionNum += 1
    logging.info("Connection established. Total: {0}".format(self.factory.connectionNum))

  def lineReceived(self, line):
    logging.info("Line received: {0}".format(line))
    components = line.split()

    if len(components) < 1:
      self.commandFailed(line)
      return

    if (components[0] == "IAMAT"):
      self.processIamAt(line)
    elif (components[0] == "WHATSAT"):
      self.processWhatsAt(line)
    elif (components[0] == "AT"):
      self.processAt(line)
    else:
      self.commandFailed(line)
    return

  def commandFailed(self, line, appendix = ""):
    logging.info("Invalid command: " + line + " " + appendix)
    self.transport.write("? " + line + "\n")
    return

  def processIamAt(self, line):
    components = line.split()
    if len(components) != 4:
      self.commandFailed(line)
      return

    clientId = components[1]
    clientPos = components[2]
    clientTime = components[3]
    
    try:
      timeDiff = time.time() - float(clientTime)
    except Exception, e:
      self.commandFailed(line, "IAMAT: Invalid input parameter")
      return

    if timeDiff >= 0:
      response = "AT {0} +{1} {2}".format(self.factory.serverName, timeDiff, ' '.join(components[1:]))
    else:
      response = "AT {0} {1} {2}".format(self.factory.serverName, timeDiff, ' '.join(components[1:]))

    self.transport.write(response + "\n")
    logging.info("Responded to IAMAT with: " + response)
    
    # We will consider a duplicate message from the client as legal, but we won't propagate it
    if (clientId in self.factory.clients) and (clientTime <= self.factory.clients[clientId]["time"]):
      logging.info("Duplicate or outdated AT info " + line)
      return
    self.factory.clients[clientId] = {"response": response, "time": clientTime}

    # Consideration of on-demand location query propagation, or servers sync'ed all the time
    self.propagate(response)

  def processWhatsAt(self, line):
    components = line.split()
    if len(components) != 4:
      self.commandFailed(line)
      return
    
    clientId = components[1]
    try:
      radius = int(components[2])
      upperBound = int(components[3])
    except Exception, e:
      self.commandFailed(line, "WHATSAT: Invalid input parameter")
      return

    if radius > 50 or upperBound > 20:
      self.commandFailed(line, "WHATSAT: range or item limit exceeded.")
      return
 
    # Consideration of client ID not found: is it treated as a invalid command, 
    #   or server should return nothing
    #   or server should explicitly say "no such client ID"
    if not (clientId in self.factory.clients):
      self.commandFailed(line, "WHATSAT: client ID not found.")
      return

    atMsg = self.factory.clients[clientId]["response"]

    try:
      clientPos = atMsg.split()[4]
      queryPos = clientPos.replace('+', ' +').replace('-', ' -').strip().replace(' ', ',')

      queryUrl = "{0}location={1}&radius={2}&sensor=false&key={3}".format(GOOGLE_PLACE_API_PREFIX, queryPos, str(radius), GOOGLE_PLACE_API_KEY)
      logging.info("Google places query URL: {0}".format(queryUrl))
      queryResponse = getPage(queryUrl)
      # One way to bind additional parameters to a callback is to use lambdas
      #  http://stackoverflow.com/questions/173687/is-it-possible-to-pass-arguments-into-event-bindings
      queryResponse.addCallback(callback = lambda x:(self.processGooglePlacesQuery(x, atMsg, upperBound, queryUrl)))
    except Exception, e:
      logging.error('Error: Google API query failed; or illegal user input: ' + str(e))
  
  # This query is async, meaning that place update could come during this query
  # Number of responses specification, different from Youtube API where a maxResults and next page token is present?
  def processGooglePlacesQuery(self, queryResponse, atMsg, upperBound, queryUrl):
    logging.debug("Google places query reply: " + queryResponse)
    responseObj = json.loads(queryResponse)
    results = responseObj["results"]
    responseObj["results"] = results[0:upperBound]
    returnMsg = "{0}\n{1}\n\n".format(atMsg, json.dumps(responseObj, indent=4))
    self.transport.write(returnMsg)
    logging.info("Responded to IAMAT with: " + atMsg + "; and Google Places query: " + queryUrl)
  
  # Routing loop
  def processAt(self, line):
    components = line.split()
    if len(components) != 7:
      self.commandFailed(line)
      return

    clientId = components[3]
    clientTime = components[5]
    senderName = components[6]

    # Check time stamp to stop flooding
    if (clientId in self.factory.clients) and (clientTime <= self.factory.clients[clientId]["time"]):
      logging.info("Duplicate or outdated AT info " + line)
      return

    self.factory.clients[clientId] = {"response": ' '.join(components[:-1]), "time": clientTime}
    logging.info("Added or updated {0} : {1}".format(clientId, self.factory.clients[clientId]["response"]))
    # Knowing who you received this message from
    self.propagate(self.factory.clients[clientId]["response"], senderName)
    return

  def propagate(self, line, exclude = ''):
    atMsg = line + ' ' + self.factory.serverName
    for neighbor in neighbors[self.factory.serverName]:
      # Make connection every time vs not
      if neighbor != exclude:
        if neighbor in self.factory.connectedServers:
          # queue messages that failed to send; or query neighbors for latest info upon startup
          self.factory.connectedServers[neighbor].sendAtMsg(atMsg)
          logging.info("Location update sent from {0} to {1}".format(self.factory.serverName, neighbor))
        else:
          reactor.connectTCP(servers[neighbor]["ip"], servers[neighbor]["port"], ProxyClient(self.factory, neighbor, atMsg))
          logging.info("Location update sent from {0} to {1}".format(self.factory.serverName, neighbor))
    return

  def connectionLost(self, reason):
    self.factory.connectionNum = self.factory.connectionNum - 1
    logging.info("Connection lost with a client. Remaining: {0}".format(self.factory.connectionNum))

class ProxyServer(protocol.ServerFactory):
  def __init__(self, serverName, serverPort):
    self.serverName = serverName
    self.serverPort = serverPort
    self.connectionNum = 0
    self.clients = {}
    self.connectedServers = {}

    self.logFile = "server-" + self.serverName + ".log"
    logging.basicConfig(filename = self.logFile, level = logging.DEBUG, filemode = 'a', format='%(asctime)s %(message)s')
    logging.info('{0}:{1} server started'.format(self.serverName, self.serverPort))

  def buildProtocol(self, addr):
    return ProxyServerProtocol(self)

  def stopFactory(self):
    logging.info("{0} server shutdown".format(self.serverName))

class ProxyClientProtocol(LineReceiver):
  def __init__ (self, factory):
    self.factory = factory

  def connectionMade(self):
    self.factory.serverObj.connectedServers[self.factory.serverName] = self.factory
    logging.info("Connection from client: {0} to server: {1} established.".format(self.factory.serverObj.serverName, self.factory.serverName))
    self.sendLine(self.factory.startupMessage)

  def connectionLost(self, reason):
    # it seems that connectionLost is called before clientConnectionLost
    if self.factory.serverName in self.factory.serverObj.connectedServers:
      del self.factory.serverObj.connectedServers[self.factory.serverName]
      logging.info("Connection from client: {0} to server: {1} lost.".format(self.factory.serverObj.serverName, self.factory.serverName)) 
    return

class ProxyClient(protocol.ClientFactory):
  def __init__(self, serverObj, serverName, startupMessage):
    # cyclic reference
    self.serverObj = serverObj
    self.serverName = serverName
    self.startupMessage = startupMessage
    return

  def buildProtocol(self, addr):
    self.protocol = ProxyClientProtocol(self)
    return self.protocol

  def sendAtMsg(self, atMsg):
    try:
      self.protocol.sendLine(atMsg)
    except Exception, e:
      logging.error("Error: client sendAtMsg error: " + str(e))
    return
  
  # Asymmetric API design; we only expect one of the connectionLost and clientConnectionLost to be 'actually' executed because of the dictionary key judgment
  def clientConnectionLost(self, connector, reason):
    if self.serverName in self.serverObj.connectedServers:
      del self.serverObj.connectedServers[self.serverName]
      logging.info("Connection from client: {0} to server: {1} lost.".format(self.serverObj.serverName, self.serverName))
    return

  def clientConnectionFailed(self, connector, reason):
    logging.info("Connection from client: {0} to server: {1} failed.".format(self.serverObj.serverName, self.serverName))
    return

def usage():
  print "Usage: python server.py [Server name]; The built-in server names are Alford, Bolden, Hamilton, Parker and Powell."
  return

def main():
  if len(sys.argv) != 2:
    usage()
    exit()
  
  serverName = sys.argv[1]
  try:
    if serverName in servers:
      factory = ProxyServer(serverName, servers[serverName]["port"])
      reactor.listenTCP(servers[serverName]["port"], factory)
      reactor.run()
    else:
      print "Error: server name not recognized from configuration"
      usage()
  except KeyError:
    print "Error: unexpected configuration format"

if __name__ == '__main__':
    main()