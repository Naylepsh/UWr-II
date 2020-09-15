public class List<T extends Comparable<T>>{
    private Element<T> begin;

    public List(){
        begin = null;
    }

    public void add(T item){
        Element<T> element = new Element<>(item);
        begin = add(element, begin);
    }

    private Element<T> add(Element<T> element, Element<T> xs){
        if (xs == null){
            return element;
        }
        if (element.val.compareTo(xs.val) != 1){
            element.next = xs;
            return element;
        }
        xs.next = add(element, xs.next);
        return xs;
    }

    public T pop() throws Exception{
        if (begin == null)
            throw new Exception("Queue is empty");
        Element<T> element = begin;
        begin = begin.next;
        return element.val;
    }

    @Override
    public String toString() {
        String s = "";
        Element<T> temp = begin;
        while (temp != null){
            s += temp.val + "\n";
            temp = temp.next;
        }
        return s;
    }
}
