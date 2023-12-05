# Challenge2 Figures Code

Code to generate Challenge 2 figures for LRGASP paper.  The data is available from Sage Synapse. 

To generate the figures, obtain code from GitHub:

```
git clone git@github.com:LRGASP/Challenge2_Figures_Code.git
cd Challenge2_Figures_Code
```


Download the data files from:

```
https://cgl.gi.ucsc.edu/data/LRGASP/paper/Challenge2_Figures_Data.zip
```

Run the R programs to build figures into the `output` directory:

```
unzip -q Challenge2_Figures_Data.zip
Rscript Code_figures_Challenge2_LRGASP_paper_figure.R
Rscript Code_figures_Challenge2_LRGASP_supplementary_figures.R 
```



