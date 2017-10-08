package gabfeed2.hashmap;


import java.util.AbstractMap;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;


public class HashMap<K, V> extends AbstractMap<K, V> {
    transient Node<K, V>[] table;
    static final transient int DEFAULT_INITIAL_CAPACITY = 16;
    static final float DEFAULT_LOAD_FACTOR = 0.75F;
    static final int MAXIMUM_CAPACITY = 1073741824;
    static final int MIN_TREEIFY_CAPACITY = 64;
    static final int TREEIFY_THRESHOLD = 8;
    float loadFactor;
    transient int capacity;
    int threshold;
    transient Set<Entry<K, V>> entrySet;
    transient int size;

    public HashMap() {
        this(16, 0.75F);
    }

    public HashMap(Map<? extends K, ? extends V> m) {
        this();
        this.putAll(m);
    }

    public HashMap(int capacity) {
        this(capacity, 0.75F);
    }

    public HashMap(int capacity, float loadFactor) {
        this.loadFactor = 0.75F;
        this.capacity = 16;
        this.threshold = 0;
        this.entrySet = new TreeSet(new HashMap.NodeComparator());
        this.size = 0;
        this.capacity = capacity;
        this.loadFactor = loadFactor;
        Node<K, V>[] newTable = (Node[])(new Node[capacity]);
        this.table = newTable;
    }

    public Set<Entry<K, V>> entrySet() {
        return this.entrySet;
    }

    public Long resizeNum = 0L;

    public V put(K key, V value) {
        V e = null;
        int h = this.hash(key);
        Node<K, V> node = this.table[h];
        if (node == null) {
            node = new Node(this.hash(key), key, value, (Node)null);
            this.table[h] = node;
            this.entrySet.add(node);
            ++this.size;
        } else if (node instanceof TreeNode) {

            TreeNode<K, V> treeNode = (TreeNode)node;
            TreeNode<K, V> result = treeNode.putTreeVal(this.table, this.hash(key), key, value);
            if (result == null) {
                this.entrySet.add(new SimpleEntry(key, value));
                ++this.size;
                return null;
            }

            if (result.value != value) {
                this.entrySet.remove(result);
                e = result.value;
                result.setValue(value);
                this.entrySet.add(result);
            }
        } else {
            int bincount;
            for(bincount = 0; node.next != null && !node.key.equals(key); ++bincount) {
                node = node.next;
            }

            if (node.key.equals(key)) {
                e = node.value;
                node.value = value;
            } else {
                this.putHelper(node, value, bincount, h, key);
            }
        }

        if ((float)this.size > (float)this.capacity * 0.75F && this.size < 1073741824) {
            this.resize();
        }

        return e;
    }

    public V get(Object key) {
        int h = this.hash((K)key);
        Node<K, V> node = this.table[h];
        if (node == null) {
            return null;
        } else if (node instanceof TreeNode) {
            TreeNode<K, V> n = (TreeNode)node;
            n = n.getTreeNode(h, key);
            return n == null ? null : n.getValue();
        } else {
            while(node.next != null && !node.key.equals(key)) {
                node = node.next;
            }

            return node.key.equals(key) ? node.value : null;
        }
    }

    public boolean containsKey(Object key) {
        V val = this.get(key);
        return val != null;
    }

    public V remove(Object key) {
        int h = this.hash((K)key);
        Node<K, V> node = this.table[h];
        Node<K, V> prev = null;
        if (node == null) {
            return null;
        } else if (node instanceof TreeNode) {
            TreeNode<K, V> treenode = (TreeNode)node;
            TreeNode<K, V> nodeToRemove = treenode.getTreeNode(h, key);
            if (nodeToRemove == null) {
                return null;
            } else {
                nodeToRemove.removeTreeNode(this.table);
                --this.size;
                this.entrySet.remove(new SimpleEntry(key, nodeToRemove.value));
                return treenode.value;
            }
        } else {
            while(node.next != null && !node.key.equals(key)) {
                prev = node;
                node = node.next;
            }

            if (node.key.equals(key)) {
                if (prev == null) {
                    this.removeHelper(node, h);
                } else {
                    this.removeHelper1(node, prev);
                }

                this.entrySet.remove(node);
                --this.size;
                return node.value;
            } else {
                return null;
            }
        }
    }

    private int hash(K key) {
        return Node.hash(key, this.capacity);
    }

    final Node<K, V>[] resize() {
        Node<K, V>[] oldTab = this.table;
        int oldCap = oldTab == null ? 0 : oldTab.length;
        int oldThr = this.threshold;
        int newThr = 0;
        int newCap;
        if (oldCap > 0) {
            if (oldCap >= 1073741824) {
                this.threshold = 2147483647;
                return oldTab;
            }

            if ((newCap = oldCap << 1) < 1073741824 && oldCap >= 16) {
                newThr = oldThr << 1;
            }
        } else if (oldThr > 0) {
            newCap = oldThr;
        } else {
            newCap = 16;
            newThr = 12;
        }

        if (newThr == 0) {
            float ft = (float)newCap * this.loadFactor;
            newThr = newCap < 1073741824 && ft < 1.07374182E9F ? (int)ft : 2147483647;
        }

        this.threshold = newThr;
        Node<K, V>[] newTab = (Node[])(new Node[newCap]);
        this.capacity = newCap;
        this.table = newTab;
        if (oldTab != null) {
            this.resizeHelper();
        }

        return newTab;
    }

    private void treeify(Node<K, V>[] tab, int index) {
        int n;
        if (tab != null && (n = tab.length) >= 64) {
            Node e;
            if ((e = tab[index]) != null) {
                TreeNode<K, V> hd = null;
                TreeNode tl = null;

                do {
                    TreeNode<K, V> p = new TreeNode(e.hash, e.key, e.value, (Node)null);
                    if (tl == null) {
                        hd = p;
                    } else {
                        this.treeifyHelper(tl, p);
                    }

                    tl = p;
                } while((e = e.next) != null);

                if ((tab[index] = hd) != null) {
                    hd.treeify(tab);
                }
            }
        } else {
            this.resize();
        }

    }

    private void putHelper(Node<K, V> node, V value, int bincount, int h, K key) {
        node.next = new Node(this.hash(key), key, value, (Node)null);
        this.entrySet.add(node.next);
        if (bincount > 8) {
            this.treeify(this.table, h);
        }

        ++this.size;
    }

    private void removeHelper(Node<K, V> node, int h) {
        this.table[h] = node.next;
    }

    private void removeHelper1(Node<K, V> node, Node<K, V> prev) {
        prev.next = node.next;
    }

    private final void resizeHelper() {
        Set<Entry<K, V>> oldEntries = this.entrySet();
        this.entrySet = new TreeSet(new HashMap.NodeComparator());
        Iterator i$ = oldEntries.iterator();

        while(i$.hasNext()) {
            Entry<K, V> entry = (Entry)i$.next();
            this.put(entry.getKey(), entry.getValue());
        }

    }

    private void treeifyHelper(TreeNode<K, V> tl, TreeNode<K, V> p) {
        p.prev = tl;
        tl.next = p;
    }

    class NodeComparator implements Comparator {
        NodeComparator() {
        }

        public int compare(Object a, Object b) {
            if (a instanceof Entry && b instanceof Entry) {
                Entry ae = (Entry)a;
                Entry be = (Entry)b;
                Object ak = ae.getKey();
                Object av = ae.getValue();
                Object bk = be.getKey();
                Object bv = be.getValue();
                if (ak.equals(bk) && (av == null && bv == null || av.equals(bv))) {
                    return 0;
                } else {
                    int avHash = 0;
                    int bvHash = 0;
                    if (av != null) {
                        avHash = av.hashCode();
                    }

                    if (bv != null) {
                        bvHash = bv.hashCode();
                    }

                    return ak.hashCode() < bk.hashCode() || ak.hashCode() == bk.hashCode() && avHash < bvHash ? -1 : 1;
                }
            } else {
                return Integer.compare(a.hashCode(), b.hashCode());
            }
        }
    }
}
