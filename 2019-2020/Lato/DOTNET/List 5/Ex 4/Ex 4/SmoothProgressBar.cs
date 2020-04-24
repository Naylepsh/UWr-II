using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Ex_4
{
    public partial class SmoothProgressBar : UserControl
    {
        private int _min;
        private int _max;
        private int _value;
        private Color BarColor = Color.Blue;

        protected override void OnPaint(PaintEventArgs e)
        {
            Graphics graphics = e.Graphics;
            SolidBrush brush = new SolidBrush(BarColor);
            float percent = (float)(_value - _min) / (float)(_max - _min);
            Rectangle rect = this.ClientRectangle;

            // Calculate area for drawing the progress.
            rect.Width = (int)((float)rect.Width * percent);

            // Draw the progress meter.
            graphics.FillRectangle(brush, rect);

            // Draw a three-dimensional border around the control.
            Draw3DBorder(graphics);

            // Clean up.
            brush.Dispose();
            graphics.Dispose();
        }

        public int Minimum
        {
            get
            {
                return _min;
            }

            set
            {
                if (value < 0)
                {
                    _min = 0;
                }

                if (value > _max)
                {
                    _min = _max;
                }

                _value = _min;

                // Invalidate the control to get a repaint.
                this.Invalidate();
            }
        }

        public int Maximum
        {
            get
            {
                return _max;
            }

            set
            {
                if (value < _min)
                {
                    value = value >= 0 ? value : 0;
                    _min = value;
                }

                _max = value;

                if (_value > _max)
                {
                    _value = _max;
                }

                // Invalidate the control to get a repaint.
                this.Invalidate();
            }
        }

        public int Value
        {
            get
            {
                return _value;
            }

            set
            {
                int oldValue = _value;

                if (value < _min)
                {
                    _value = _min;
                }
                else if (value > _max)
                {
                    _value = _max;
                }
                else
                {
                    _value = value;
                }

                var updateRect = updateBar(oldValue);

                // Invalidate the intersection region only.
                this.Invalidate(updateRect);
            }
        }

        private Rectangle updateBar(int oldValue)
        {
            // Invalidate only the changed area.
            float percent;

            Rectangle newValueRect = this.ClientRectangle;
            Rectangle oldValueRect = this.ClientRectangle;

            // Use a new value to calculate the rectangle for progress.
            percent = (float)(_value - _min) / (float)(_max - _min);
            newValueRect.Width = (int)((float)newValueRect.Width * percent);

            // Use an old value to calculate the rectangle for progress.
            percent = (float)(oldValue - _min) / (float)(_max - _min);
            oldValueRect.Width = (int)((float)oldValueRect.Width * percent);

            Rectangle updateRect = new Rectangle();

            updateRect.Height = this.Height;

            // Find only the part of the screen that must be updated.
            if (newValueRect.Width > oldValueRect.Width)
            {
                updateRect.X = oldValueRect.Size.Width;
                updateRect.Width = newValueRect.Width - oldValueRect.Width;
            }
            else
            {
                updateRect.X = newValueRect.Size.Width;
                updateRect.Width = oldValueRect.Width - newValueRect.Width;
            }

            return updateRect;
        }

        public Color ProgressBarColor
        {
            get
            {
                return BarColor;
            }

            set
            {
                BarColor = value;

                // Invalidate the control to get a repaint.
                this.Invalidate();
            }
        }

        private void Draw3DBorder(Graphics graphics)
        {
            int PenWidth = (int)Pens.White.Width;

            graphics.DrawLine(Pens.DarkGray,
            new Point(this.ClientRectangle.Left, this.ClientRectangle.Top),
            new Point(this.ClientRectangle.Width - PenWidth, this.ClientRectangle.Top));
            graphics.DrawLine(Pens.DarkGray,
            new Point(this.ClientRectangle.Left, this.ClientRectangle.Top),
            new Point(this.ClientRectangle.Left, this.ClientRectangle.Height - PenWidth));
            graphics.DrawLine(Pens.White,
            new Point(this.ClientRectangle.Left, this.ClientRectangle.Height - PenWidth),
            new Point(this.ClientRectangle.Width - PenWidth, this.ClientRectangle.Height - PenWidth));
            graphics.DrawLine(Pens.White,
            new Point(this.ClientRectangle.Width - PenWidth, this.ClientRectangle.Top),
            new Point(this.ClientRectangle.Width - PenWidth, this.ClientRectangle.Height - PenWidth));
        }

        public SmoothProgressBar()
        {
            InitializeComponent();
        }
    }
}
