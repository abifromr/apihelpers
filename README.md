[![Travis build status](https://travis-ci.org/abifromr/apihelpers.svg?branch=master)](https://travis-ci.org/abifromr/apihelpers)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/abifromr/apihelpers?branch=master&svg=true)](https://ci.appveyor.com/project/abifromr/apihelpers)
[![Coverage status](https://codecov.io/gh/abifromr/apihelpers/branch/master/graph/badge.svg)](https://codecov.io/github/abifromr/apihelpers?branch=master)

# apihelpers

Helper Functions for Making an R Client for an API

## Overview

Provide functions for constructing a URL, sending requests and capturing API 
responses. The query parameters are checked and formatted according to a 
`query_map` which describes the type of requests and queries allowed by the 
API.

## Installation

Install `apihelpers` from GitHub using `devtools`

```r
devtools::install_github('abifromr/apihelpers')
```
