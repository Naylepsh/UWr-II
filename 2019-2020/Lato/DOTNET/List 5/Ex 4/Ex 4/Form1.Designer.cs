namespace Ex_4
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.timer1 = new System.Windows.Forms.Timer(this.components);
            this.button1 = new System.Windows.Forms.Button();
            this.smoothProgressBar1 = new Ex_4.SmoothProgressBar();
            this.SuspendLayout();
            // 
            // timer1
            // 
            this.timer1.Tick += new System.EventHandler(this.timer1_Tick);
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(157, 82);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(94, 37);
            this.button1.TabIndex = 1;
            this.button1.Text = "Click to progress";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // smoothProgressBar1
            // 
            this.smoothProgressBar1.Location = new System.Drawing.Point(26, 36);
            this.smoothProgressBar1.Maximum = 100;
            this.smoothProgressBar1.Minimum = 0;
            this.smoothProgressBar1.Name = "smoothProgressBar1";
            this.smoothProgressBar1.ProgressBarColor = System.Drawing.Color.Blue;
            this.smoothProgressBar1.Size = new System.Drawing.Size(369, 40);
            this.smoothProgressBar1.TabIndex = 0;
            this.smoothProgressBar1.Value = 0;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(416, 127);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.smoothProgressBar1);
            this.Name = "Form1";
            this.Text = "Smooth Progress Bar";
            this.ResumeLayout(false);

        }

        #endregion

        private SmoothProgressBar smoothProgressBar1;
        private System.Windows.Forms.Timer timer1;
        private System.Windows.Forms.Button button1;
    }
}