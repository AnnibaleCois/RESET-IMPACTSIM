<br/>
<br/>
<img src="www/logo_combined.png" alt="Vital Strategies & UCT logos" width="900">
<br/>
<br/>
<h1>RESET Alcohol</h1>

<p>
  <em>Lives saved and disease burden averted through reduced alcohol consumption in South Africa.</em>
</p>

---

[![DOI](https://zenodo.org/badge/1387008282.svg)](https://doi.org/10.5281/zenodo.23855384)


## Averted Burden of Disease Simulator 

A Shiny app for visualising changes in burden of disease (deaths, YLL, YLD and DALYs) under hypothetical changes in the population patterns of alcohol consumption in the South African adult population.

Users specify hypothetical changes by population strata, run the simulation, and explore the results as interactive charts and tables.

## Repository Structure

```
.
├── data/          # Precalculated burden and distribution estimates and auxiliary data
├── www/           # Static files 
├── global.R       # Code executed at app start (shared across users)
├── ui.R           # User Interface
├── server.R       # Server
├── functions.R    # Functions for pif calculations (used by Server)
├── renv.lock      # Pinned package versions
├── renv/          # renv project infrastructure
└── README.md
```

## Requirements

Package versions are managed with [renv](https://rstudio.github.io/renv/).
The `renv.lock` file in this repo pins the exact versions used, including:

- shiny
- bslib
- DT
- ggplot2
- plotly
- dplyr
- tidyr
- scales

No manual `install.packages()` calls are needed — see **Getting started** below.

## Getting started

1. Clone the repo. 

2. Open the project in R (e.g. via the `.Rproj` file, if present) so `renv`
   activates automatically. If it doesn't activate on its own, run:

   ```r
   renv::activate()
   ```

3. Restore the exact package versions pinned in `renv.lock`:

   ```r
   renv::restore()
   ```

5. Run the app:

   ```r
   shiny::runApp()
   ```

## Citation

If you use this application in your research, project, or publication, please cite it as follows:

#### APA Style
> Cois A, Vellios N, Chikanya M, Matzopoulos R (2026). *IMPACTSIM* (Version 1.0.0) [Computer software]. DOI: 10.5281/zenodo.23855384. https://github.com/AnnibaleCois/RESET-IMPACTSIM

#### BibTeX
```bibtex
@software{IMPACTSIM2026,
  author = {Cois, Annibale and Vellios, Nicole and Chikanya, MAgnolia and Matzopoulos, Richard},
  title = {{IMPACTSIM}},
  year = {2026},
  version = {1.0.0},
  doi = {10.5281/zenodo.23855384},
  url = {[https://github.com/AnnibaleCois/RESET-IMPACTSIM](https://github.com/AnnibaleCois/RESET-IMPACTSIM)}
}
```
## License

<!-- MIT License Badge (Optional) -->
<p>
  <a href="https://opensource.org/licenses/MIT">
    <img src="https://img.shields.io/badge/License-MIT-yellow.svg" alt="License: MIT">
  </a>
</p>

<p>Copyright &copy;2026-<span id="current-year">2026</span> Annibale Cois, University of Cape Town, <a href="mailto:annibale.cois@uct.ac.za"> annibale.cois@uct.ac.za</a></p>

<p>Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the &quot;Software&quot;), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:</p>

<p>The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.</p>

<p><strong>THE SOFTWARE IS PROVIDED &quot;AS IS&quot;, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.</strong></p>

## Contact

Annibale Cois, <a href="mailto: annibale.cois@uct.ac.za">annibale.cois@uct.ac.za</a>
