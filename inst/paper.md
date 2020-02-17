---
title: 'Economiccomplexity: Computational Methods for Economic Complexity'
authors:
- affiliation: 1
  name: Mauricio Vargas
  orcid: 0000-0003-1017-7574
date: "23 August 2018"
output: pdf_document
bibliography: REFERENCES.bib
tags:
- R
- economic complexity
- international trade
- income inequality
- migration
affiliations:
- index: 1
  name: Pontifical Catholic University of Chile
---

# Summary

Economic complexity introduces network theory concepts to different social 
science considerations related to international trade and income inequality. 
With the bulk of literature established in the last decade, the field of 
economic complexity is relatively new. Its approach starts from representing 
international trade data as a bipartite network that connects countries to the 
products that they export.

Seminal papers in economic complexity are: @productspace2007, which introduces 
graphs to explore trade diversification and export opportunities; and 
@buildingblocks2009, which develops dedicated metrics of complexity. These two
articles are expanded and interpreted in @atlas2014, whose typesetted equations
were translated to code in the R package **economiccomplexity**.

This R package provides different methods to compute complexity metrics that ease
access to this line of research for social scientists. `economiccomplexity::`
depends on `Matrix::` and `igraph::`, and
follows the same design philosophy, grammar, and data structures from it (See
@matrix and @igraph).

Recent articles such as @linking2017 introduce the question whereas a country's
mix of products could predict its pattern of diversification and income
inequality, but do not include linked and executable code and data. This
separation between the research, the complete process that produced the
results, and its presentation, makes it difficult for others to verify the
findings in the study.

`economiccomplexity::` might help to evaluate research findings in this 
particular area, reducing the number of studies that are not reproducible, or 
only partially reproducible with some discrepancies, conditional on the 
availability of data, metadata, and computing power that may be unavailable to 
all researchers.

The central contribution of `economiccomplexity::` is to provide functions that 
use recursive linear algebra methods from @buildingblocks2009 in R, with tests 
and full documentation for release on CRAN, the dominant repository of R 
software.

@metrics2012 presents non-linear iterative methods that extend
the linear approach from @buildingblocks2009 in order to capture the link
between the export basket of different countries and their industrial
competitiveness. This approach is also implemented in `economiccomplexity::`, but
following the formulation from @measuringcomplexity2015 that introduces
extremality parameters that generalize the original formulation.

@reproducible2011 states that reproducibility has the potential to serve as a
minimum standard when full independent replication of a study is not possible,
and that becomes even more important to evaluate scientific claims in studies
with public policy implications. A tenet of the scientific method holds that
every research finding should be reproducible before it becomes accepted as a
genuine contribution to human knowledge (@computing2009).

Complexity methods are also used outside its original research area. In
particular, the fitness method from @metrics2012 has been used in ecology to
study species interaction (see @ranking2015) and, more directly related, to
study the scientific competitiveness of nations (See @scientific2014).

The application widespread of economic complexity should be considered with 
caution. Any theoretical or computational implementation shall be far
from becoming a keystone if reproducibility is ignored. The extent to which code
in computational research would build with reasonable effort is lower than 20%
(See @measuring2014). A desirable growth pattern should focus on reproducibility,
and I hope this package means a contribution to transparent research
practices.

@atlas2014 proposes the equivalency between the reflections and 
the eigenvalues methods that they present.My code on GitHub
checks for the sign of the correlation for the vector output of the two methods and 
corrects the eigenvalues output when necessary. What motivated those additional
steps is that, what I initially tought it was an error, turned out to be a 
particular case that emerges with some datasets. Shortly after implementing that,
both Python and Stata users started sending me emails when they, independently,
found the same problem and  then search engines lead them to my package.

# References
