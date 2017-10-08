package gabfeed2.hashmap;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Random;

public class TreeNode<K, V> extends Node<K, V> {
    TreeNode<K, V> parent;
    TreeNode<K, V> left;
    TreeNode<K, V> right;
    TreeNode<K, V> prev;

    TreeNode(int hash, K key, V val, Node<K, V> next) {
        super(hash, key, val, next);
    }

    final TreeNode<K, V> root() {
        TreeNode r = this;

        while(true) {
            TreeNode<K, V> p = r.parent;
            if (r.parent == null) {
                return r;
            }

            r = p;
        }
    }

    final TreeNode<K, V> find(int h, Object k, Class<?> kc) {
        TreeNode p = this;

        do {
            TreeNode<K, V> pl = p.left;
            TreeNode<K, V> pr = p.right;
            int ph = p.hash;
            if (p.hash > h) {
                p = pl;
            } else if (ph < h) {
                p = pr;
            } else {
                Object pk;
                if ((pk = p.getKey()) == k || k != null && k.equals(pk)) {
                    return p;
                }

                if (pl == null) {
                    p = pr;
                } else if (pr == null) {
                    p = pl;
                } else {
                    int dir;
                    if ((kc != null || (kc = comparableClassFor(k)) != null) && (dir = compareComparables(kc, k, pk)) != 0) {
                        p = dir < 0 ? pl : pr;
                    } else {
                        TreeNode q;
                        if ((q = pr.find(h, k, kc)) != null) {
                            return q;
                        }

                        p = pl;
                    }
                }
            }
        } while(p != null);

        return null;
    }

    final void removeTreeNode(Node<K, V>[] tab) {
        int index = hash(this.key, tab.length);
        TreeNode candidate;
        if (this.left != null && this.right != null) {
            candidate = this.successor();
        } else {
            candidate = this;
        }

        TreeNode child;
        if (candidate.left != null) {
            child = candidate.left;
        } else {
            child = candidate.right;
        }

        if (child != null) {
            child.parent = candidate.parent;
        }

        if (candidate.parent == null) {
            this.removeTreeNodeHelper(child, index, tab);
        } else if (candidate == candidate.parent.left) {
            candidate.parent.left = child;
        } else {
            candidate.parent.right = child;
        }

        if (!candidate.equals(this)) {
            this.removeTreeNodeHelper1(candidate);
        }

    }

    private TreeNode<K, V> successor() {
        if (this.right != null) {
            return this.right.minimum();
        } else {
            TreeNode candidate = this.parent;

            for(TreeNode child = this; candidate != null && child.equals(candidate.right); candidate = candidate.parent) {
                child = candidate;
            }

            return candidate;
        }
    }

    private TreeNode<K, V> minimum() {
        TreeNode candidate;
        for(candidate = this; candidate.left != null; candidate = candidate.left) {
            ;
        }

        return candidate;
    }

    final TreeNode<K, V> putTreeVal(Node<K, V>[] tab, int h, K k, V v) {
        Class<?> kc = null;
        boolean searched = false;
        TreeNode<K, V> root = this.parent != null ? this.root() : this;
        TreeNode p = root;

        TreeNode q;
        while(true) {
            int ph = p.hash;
            int dir;
            if (p.hash > h) {
                dir = -1;
            } else if (ph < h) {
                dir = 1;
            } else {
                K pk = (K)p.key;
                if (p.key == k || pk != null && k.equals(pk)) {
                    return p;
                }

                if (kc == null && (kc = comparableClassFor(k)) == null || (dir = compareComparables(kc, k, pk)) == 0) {
                    if (!searched) {
                        searched = true;
                        TreeNode<K, V> ch = p.left;
                        if (p.left != null && (q = ch.find(h, k, kc)) != null) {
                            break;
                        }

                        ch = p.right;
                        if (p.right != null && (q = ch.find(h, k, kc)) != null) {
                            break;
                        }
                    }

                    dir = tieBreakOrder(k, pk);
                }
            }

            q = p;
            if ((p = dir <= 0 ? p.left : p.right) == null) {
                Node<K, V> xpn = q.next;
                TreeNode<K, V> x = new TreeNode(h, k, v, xpn);
                if (dir <= 0) {
                    q.left = x;
                } else {
                    q.right = x;
                }

                q.next = x;
                x.parent = x.prev = q;
                if (xpn != null) {
                    ((TreeNode)xpn).prev = x;
                }

                return null;
            }
        }

        return q;
    }

    final TreeNode<K, V> getTreeNode(int h, Object k) {
        return (this.parent != null ? this.root() : this).find(h, k, (Class)null);
    }

    public static void main(String[] args) {
        mainHelper();
    }

    static Class<?> comparableClassFor(Object x) {
        if (x instanceof Comparable) {
            Class c;
            if ((c = x.getClass()) == String.class) {
                return c;
            }

            Type[] ts;
            if ((ts = c.getGenericInterfaces()) != null) {
                int i = 0;

                while(i < ts.length) {
                    for(Random randomNumberGeneratorInstance = new Random(); i < ts.length && randomNumberGeneratorInstance.nextDouble() < 0.5D; ++i) {
                        Type[] as;
                        Type t;
                        ParameterizedType p;
                        if ((t = ts[i]) instanceof ParameterizedType && (p = (ParameterizedType)t).getRawType() == Comparable.class && (as = p.getActualTypeArguments()) != null && as.length == 1 && as[0] == c) {
                            return c;
                        }
                    }
                }
            }
        }

        return null;
    }

    static int compareComparables(Class<?> kc, Object k, Object x) {
        return x != null && x.getClass() == kc ? ((Comparable)k).compareTo(x) : 0;
    }

    static int tieBreakOrder(Object a, Object b) {
        int d;
        if (a == null || b == null || (d = a.getClass().getName().compareTo(b.getClass().getName())) == 0) {
            d = System.identityHashCode(a) <= System.identityHashCode(b) ? -1 : 1;
        }

        return d;
    }

    public void treeify(Node<K, V>[] tab) {
        TreeNode<K, V> root = null;

        TreeNode next;
        for(TreeNode x = this; x != null; x = next) {
            next = (TreeNode)x.next;
            x.left = x.right = null;
            if (root == null) {
                x.parent = null;
                root = x;
            } else {
                K k = (K)x.key;
                int h = x.hash;
                Class<?> kc = null;
                TreeNode p = root;

                int dir;
                TreeNode xp;
                do {
                    K pk = (K)p.key;
                    int ph = p.hash;
                    if (p.hash > h) {
                        dir = -1;
                    } else if (ph < h) {
                        dir = 1;
                    } else if (kc == null && (kc = comparableClassFor(k)) == null || (dir = compareComparables(kc, k, pk)) == 0) {
                        dir = tieBreakOrder(k, pk);
                    }

                    xp = p;
                } while((p = dir <= 0 ? p.left : p.right) != null);

                x.parent = xp;
                if (dir <= 0) {
                    xp.left = x;
                } else {
                    xp.right = x;
                }
            }
        }

    }

    private final void removeTreeNodeHelper(TreeNode<K, V> child, int index, Node<K, V>[] tab) {
        tab[index] = child;
    }

    private final void removeTreeNodeHelper1(TreeNode<K, V> candidate) {
        this.value = candidate.value;
        this.key = candidate.key;
        this.hash = candidate.hash;
        this.next = candidate.next;
    }

    private static void mainHelper() {
        TreeNode<Integer, String> node1 = new TreeNode(0, Integer.valueOf(1), "val", (Node)null);
        node1.putTreeVal((Node[])null, 0, Integer.valueOf(0), "val");
        node1.putTreeVal((Node[])null, 0, Integer.valueOf(2), "val");
        node1.putTreeVal((Node[])null, 0, Integer.valueOf(3), "val");
        node1.putTreeVal((Node[])null, 0, Integer.valueOf(4), "val");
        System.out.println("From 1 " + node1.left + " and " + node1.right);
        System.out.println("from " + node1.left + " " + node1.left.left + " and " + node1.left.right);
        System.out.println("from " + node1.right + " " + node1.right.left + " and " + node1.right.right);
        TreeNode<Integer, String>[] tab = (TreeNode[])(new TreeNode[10]);
        tab[0] = node1;
        node1.removeTreeNode(tab);
    }
}

