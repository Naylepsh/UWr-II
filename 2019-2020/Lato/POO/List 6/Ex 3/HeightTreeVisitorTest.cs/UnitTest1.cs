using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ex_3;

namespace HeightTreeVisitorTest.cs
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void HeightOfLeaf()
        {
            TreeLeaf leaf = new TreeLeaf() { Value = 1 };
            HeightTreeVisitor visitor = new HeightTreeVisitor();
            visitor.Visit(leaf);
            Assert.AreEqual(visitor.Height, 0);
        }

        [TestMethod]
        public void HeightOfLeftHeavyTree()
        {
            Tree root = new TreeNode()
            {
                Left = new TreeNode()
                {
                    Left = new TreeLeaf() { Value = 1 },
                    Right = new TreeLeaf() { Value = 2 },
                },
                Right = new TreeLeaf() { Value = 3 },
            };
            HeightTreeVisitor visitor = new HeightTreeVisitor();
            visitor.Visit(root);
            Assert.AreEqual(visitor.Height, 2);
        }

        [TestMethod]
        public void HeightOfLeftRightTree()
        {
            Tree root = new TreeNode()
            {
                Right = new TreeNode()
                {
                    Left = new TreeLeaf() { Value = 1 },
                    Right = new TreeLeaf() { Value = 2 },
                },
                Left = new TreeLeaf() { Value = 3 },
            };
            HeightTreeVisitor visitor = new HeightTreeVisitor();
            visitor.Visit(root);
            Assert.AreEqual(visitor.Height, 2);
        }
    }
}
