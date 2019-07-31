public class Element<T extends Comparable<T>> implements Comparable<T> {
    public T val;
    public Element<T> next;

    protected Element(T val){
        this.val = val;
        this.next = null;
    }

    @Override
    public int compareTo(T o) {
        return val.compareTo(o);
    }
}
