# Project overview

## Data source


## Project structure

```
│projectdir          <- Project's main folder. It is initialized as a Git
│                       repository with a reasonable .gitignore file.
│
├── _research        <- WIP scripts, code, notes, comments,
│   |                   to-dos and anything in an alpha state.
│   └── tmp          <- Temporary data folder.
│
├── data             <- **Immutable and add-only!**
│
├── plots            <- Self-explanatory.
├── notebooks        <- Jupyter, Weave or any other mixed media notebooks.
│
├── papers           <- Scientific papers resulting from the project.
│
├── scripts          <- Various scripts, e.g. simulations, plotting, analysis,
│                       The scripts use the `src` folder for their base code.
│
├── src              <- Source code for use in this project. Contains functions,
│                       structures and modules that are used throughout
│                       the project and in multiple scripts.
│
├── README.md        <- Optional top-level README for anyone using this project.
├── .gitignore       <- by default ignores _research, data, plots, videos,
│                       notebooks and latex-compilation related files.
│
├── Manifest.toml    <- Contains full list of exact package versions used currently.
└── Project.toml     <- Main project file, allows activation and installation.
                        Includes DrWatson by default.
```

### Code


## Project

### Glucose prediction

gp: glucose prediction

## Data dictionary

Key variables:

time

New variables naming:  

Counting variables are indicated by the postfix '_n', e.g. glucose_n counts the number of glucose measurements

Last observations carried forward are postfixed '_locf'

The time of a measure is indicated by [measure]_[datetime]

The time gap in minutes (relative to time) to a measure value is indicated 
by [measure]_[source (e.g. 'locf')]_tdiff


Missing - we will experiment with masking based on missing in last few minutes and missing ever

no recorded measure so far is indicated as [measure]_missing_m
