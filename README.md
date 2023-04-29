# Challenge2 Figures Code

Code to generate Challenge 2 figures for LRGASP paper.  The data is available from Sage Synapse. 

To generate the figures, obtain code from GitHub:

```
git clone git@github.com:LRGASP/Challenge2_Figures_Code.git
cd Challenge2_Figures_Code
```

The data file is `Challenge2_Figures_Data.zip`, Synapse id  `syn51401823`.
Install synapseclient if necessary: `pip install synapseclient`

Download, extract, and run the R programs to build figures into the `output` directory:

```
synapse get syn51401823
unzip -q ~/tmp/Challenge2_Figures_Data.zip
Rscript Code_figures_Challenge2_LRGASP_paper_figure.R
Rscript Code_figures_Challenge2_LRGASP_supplementary_figures.R 
```



