% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShinyUtils.R
\name{DataTable}
\alias{DataTable}
\title{DataTable}
\usage{
DataTable(data, FixedCols = 2)
}
\arguments{
\item{data}{source data.table}

\item{FixedCols}{Number of columns from the left to Freeze, like freeze panes in Excel. Default is 2}
}
\description{
Fully loaded DT::datatable() with args prefilled
}
\examples{
\dontrun{
# Rmarkdown example of DataTable inside a <details> </Details> section

```{r Get Dependencies For DT::datatable(), echo=FALSE,include = FALSE}
# You need this code to conduct the magic dependences attaching...
DT::datatable(matrix())
```

```{js Nest All DT::datatable() inside a details drop down, echo=FALSE}
setTimeout(function() {
  var codes = document.querySelectorAll('.dataTables_wrapper');
  var code, i, d, s, p;
  for (i = 0; i < codes.length; i++) {
    code = codes[i];
    p = code.parentNode;
    d = document.createElement('details');
    s = document.createElement('summary');
    s.innerText = 'Details';
    // <details><summary>Details</summary></details>
      d.appendChild(s);
    // move the code into <details>
      p.replaceChild(d, code);
    d.appendChild(code);
  }
});
```

```{r Example, echo = FALSE}
Quantico:::DataTable(data)
````

# Shiny Usage
output$Table <- shiny::renderUI({Quantico:::DataTable(data)})

}

}
\seealso{
Other Utils: 
\code{\link{CleanVector}()},
\code{\link{IntraSessionDefaults}()},
\code{\link{Shiny.CodePrint.Collect}()},
\code{\link{Shiny.CodePrint.OrganizeCode}()},
\code{\link{Shiny.Utils.CachePath}()},
\code{\link{Utils.Code.Lines}()},
\code{\link{Utils.LinuxRunSH}()},
\code{\link{Utils.Nest}()}
}
\author{
Adrian Antico
}
\concept{Utils}
