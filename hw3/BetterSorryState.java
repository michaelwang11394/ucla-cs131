import java.util.concurrent.atomic.AtomicInteger;

class BetterSorryState implements State {
    private byte[] value;
    private byte maxval;

    BetterSorryState(byte[] v) { 
        value = v;
        maxval = 127; 
    }

    BetterSorryState(byte[] v, byte m) { 
        value = v;
        maxval = m; 
    }

    public int size() { return value.length; }

    public byte[] current() { 
        return value;
    }

    public boolean swap(int i, int j) {
        if (value[i] <= 0 || value[j] >= maxval) {
            return false;
        }
        
        AtomicInteger ai = new AtomicInteger(value[i]);
        AtomicInteger aj = new AtomicInteger(value[j]);

        // we want the read and write in this to be atomic; since we have two indices
        // what about the case "wanting to decrement an i, with it being increment of another thread?"
        value[i] = (byte) ai.getAndDecrement();
        value[j] = (byte) aj.getAndIncrement();
        return true;
    }
}

// This performs worse than generating atomic integer on the fly, even worse than synchronized...
/*
class BetterSorryState implements State {
    private AtomicInteger[] value;
    private byte maxval;

    BetterSorryState(byte[] v) { 
        value = new AtomicInteger[v.length];
        for (int i = 0; i < v.length; i++) {
            value[i] = new AtomicInteger(v[i]);
        }
        maxval = 127; 
    }

    BetterSorryState(byte[] v, byte m) { 
        value = new AtomicInteger[v.length];
        for (int i = 0; i < v.length; i++) {
            value[i] = new AtomicInteger(v[i]);
        }
        maxval = m; 
    }

    public int size() { return value.length; }

    public byte[] current() { 
        byte[] temp = new byte[value.length];
        for (int i = 0; i < value.length; i++) {
            temp[i] = (byte)value[i].get();
        }
        return temp;
    }

    public boolean swap(int i, int j) {
        if (value[i].get() <= 0 || value[j].get() >= maxval) {
            return false;
        }

        // we want the read and write in this to be atomic; since we have two indices
        // what about the case "wanting to decrement an i, with it being increment of another thread?"
        value[i].getAndDecrement();
        value[j].getAndIncrement();
        return true;
    }
}
*/
