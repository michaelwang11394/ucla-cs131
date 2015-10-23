import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSetState(byte[] v) { 
        value = new AtomicIntegerArray(v.length);
        for (int i = 0; i < v.length; i++) {
            value.set(i, v[i]);
        }
        maxval = 127; 
    }

    GetNSetState(byte[] v, byte m) { 
        value = new AtomicIntegerArray(v.length);
        for (int i = 0; i < v.length; i++) {
            value.set(i, v[i]);
        }
        maxval = m; 
    }

    public int size() { return value.length(); }

    public byte[] current() { 
        byte[] temp = new byte[value.length()];
        for (int i = 0; i < value.length(); i++) {
            temp[i] = (byte) value.get(i);
        }
        return temp;
    }

    public boolean swap(int i, int j) {
        if (value.get(i) <= 0 || value.get(j) >= maxval) {
            return false;
        }
        value.getAndDecrement(i);
        value.getAndIncrement(j);
        return true;
    }
}
