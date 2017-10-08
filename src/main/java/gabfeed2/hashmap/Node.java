package gabfeed2.hashmap;


import java.util.Objects;
import java.util.Map.Entry;

public class Node<K, V> implements Entry<K, V> {
    int hash;
    K key;
    V value;
    Node<K, V> next;

    Node(int hash, K key, V value, Node<K, V> next) {
        this.hash = hash;
        this.key = key;
        this.value = value;
        this.next = next;
    }

    public final K getKey() {
        return this.key;
    }

    public final V getValue() {
        return this.value;
    }

    public final String toString() {
        return this.key + "=" + this.value;
    }

    public final int hashCode() {
        return Objects.hashCode(this.key) ^ Objects.hashCode(this.value);
    }

    public static int hash(Object key, int capacity) {
        return (key.hashCode() & 2147483647) % capacity;
    }

    public final V setValue(V newValue) {
        V oldValue = this.value;
        this.value = newValue;
        return oldValue;
    }

    public final boolean equals(Object o) {
        if (o == this) {
            return true;
        } else {
            if (o instanceof Entry) {
                Entry<?, ?> e = (Entry)o;
                if (Objects.equals(this.key, e.getKey()) && Objects.equals(this.value, e.getValue())) {
                    return true;
                }
            }

            return false;
        }
    }
}
