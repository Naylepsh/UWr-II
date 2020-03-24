using System.Collections;
using System.Collections.Generic;
using System;

namespace ex2
{
    class Program
    {
        static void Main(string[] args)
        {
            BinaryTreeNode<int> leftLeft = new BinaryTreeNode<int>(1);
            BinaryTreeNode<int> leftRight = new BinaryTreeNode<int>(3);
            BinaryTreeNode<int> left = new BinaryTreeNode<int>(2, leftLeft, leftRight);

            BinaryTreeNode<int> rightLeft = new BinaryTreeNode<int>(5);
            BinaryTreeNode<int> rightRight = new BinaryTreeNode<int>(7);
            BinaryTreeNode<int> right = new BinaryTreeNode<int>(6, rightLeft, rightRight);

            BinaryTreeNode<int> root = new BinaryTreeNode<int>(4, left, right);
            foreach (var value in root) {
                Console.Write("{0},", value);
            }
        }
    }

    public class BinaryTreeNode<T> : IEnumerable {
        BinaryTreeNode<T> _leftChild;
        BinaryTreeNode<T> _rightChild;
        T _value;

        public BinaryTreeNode<T> LeftChild => _leftChild;
        public BinaryTreeNode<T> RightChild => _rightChild;
        public T Value => _value;

        public BinaryTreeNode(T value, BinaryTreeNode<T> leftChild = null, BinaryTreeNode<T> rightChild = null) {
            _value = value;
            _leftChild = leftChild;
            _rightChild = rightChild;
        }

        public IEnumerator GetEnumerator() {
            // return new BinaryTreeNodePreorderEnumerator(this);
            // return new BinaryTreeNodeBfsEnumerator(this);
            // return GetEnumeratorPreorder(this);
            return GetEnumeratorBFS(this);
        }

        public IEnumerator GetEnumeratorPreorder(BinaryTreeNode<T> root) {
            yield return root.Value;
            if (root.LeftChild != null) {
                foreach (var value in root.LeftChild) {
                    yield return value;
                }
            }
            if (root.RightChild != null) {
                foreach (var value in root.RightChild) {
                    yield return value;
                }
            }
        }

        public IEnumerator GetEnumeratorBFS(BinaryTreeNode<T> root) {
            var queue = new Queue<BinaryTreeNode<T>>();
            queue.Enqueue(root);

            while (queue.Count > 0) {
                var node = queue.Dequeue();
                yield return node.Value;
                if (node.LeftChild != null) queue.Enqueue(node.LeftChild);
                if (node.RightChild != null) queue.Enqueue(node.RightChild); 
            }
        }

        public class BinaryTreeNodePreorderEnumerator : IEnumerator {
            private Stack<BinaryTreeNode<T>> stack = new Stack<BinaryTreeNode<T>>();
            private BinaryTreeNode<T> _root;
            private BinaryTreeNode<T> _curr;

            public object Current {
                get {
                    if (_curr != null) {
                        return _curr.Value;
                    } else {
                        throw new ArgumentException("No element to return");
                    }
                }
            }

            public BinaryTreeNodePreorderEnumerator(BinaryTreeNode<T> root) {
                this._root = root;
                this.Reset();
            }

            public bool MoveNext() {
                if (stack.Count > 0) {
                    _curr = stack.Pop();
                    if (_curr.RightChild != null) stack.Push(_curr.RightChild);
                    if (_curr.LeftChild != null) stack.Push(_curr.LeftChild); 
                    return true;
                } else {
                    return false;
                }
            }

            public void Reset() {
                stack.Clear();
                stack.Push(this._root);
                this._curr = this._root;
            }
        }

        public class BinaryTreeNodeBfsEnumerator : IEnumerator {
            private Queue<BinaryTreeNode<T>> queue = new Queue<BinaryTreeNode<T>>();
            private BinaryTreeNode<T> _root;
            private BinaryTreeNode<T> _curr;

            public object Current {
                get {
                    if (_curr != null) {
                        return _curr.Value;
                    } else {
                        throw new ArgumentException("No element to return");
                    }
                }
            }

            public BinaryTreeNodeBfsEnumerator(BinaryTreeNode<T> root) {
                this._root = root;
                this.Reset();
            }

            public bool MoveNext() {
                if (queue.Count > 0) {
                    _curr = queue.Dequeue();
                    if (_curr.LeftChild != null) queue.Enqueue(_curr.LeftChild);
                    if (_curr.RightChild != null) queue.Enqueue(_curr.RightChild);
                    return true;
                } else {
                    return false;
                }
            }

            public void Reset() {
                queue.Clear();
                queue.Enqueue(this._root);
                this._curr = this._root;
            }
        }
    }
}
