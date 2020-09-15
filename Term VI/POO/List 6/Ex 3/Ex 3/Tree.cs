using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex_3
{
    public abstract class Tree{}

    public  class TreeNode : Tree
    {
        public Tree Left { get; set; }

        public Tree Right { get; set; }
    }

    public class TreeLeaf : Tree
    {
        public int Value { get; set; }
    }

    public abstract class TreeVisitor
    {
        public void Visit(Tree tree){
            if (tree is TreeNode)
                this.VisitNode((TreeNode)tree);
            if (tree is TreeLeaf)
                this.VisitLeaf((TreeLeaf)tree);
        }

        public virtual void VisitNode(TreeNode node)
        {
            if (node!=null)
            {
                this.Visit(node.Left);
                this.Visit(node.Right);
            }
        }

        public virtual void VisitLeaf(TreeLeaf leaf)
        {
        }
    }

    public class SumTreeVisitor : TreeVisitor
    {
        public int Sum { get; set; }
        public override void VisitLeaf(TreeLeaf leaf)
        {
            base.VisitLeaf(leaf);
            this.Sum += leaf.Value;
        }
    }

    public class HeightTreeVisitor : TreeVisitor
    {
        public int Height { get; set; }
        public int CurrentHeight { get; set; }

        public override void VisitNode(TreeNode node)
        {
            CurrentHeight++;
            base.VisitNode(node);
            CurrentHeight--;
        }

        public override void VisitLeaf(TreeLeaf leaf)
        {
            if (CurrentHeight > Height)
            {
                Height = CurrentHeight;
            }
            base.VisitLeaf(leaf);
        }
    }
}
