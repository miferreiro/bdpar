% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExtractorSms.R
\name{ExtractorSms}
\alias{ExtractorSms}
\title{Class to handle SMS files with tsms extension}
\description{
This class that inherits from the \code{\link{Instance}} class and
implements the functions of extracting the text and the date of an tsms type file.
}
\section{Details}{

Due to the fact that the creation date of the message can not be
extracted from the text of an SMS, the date will be initialized to empty.
}

\section{Inherit}{

This class inherits from \code{\link{Instance}} and implements the
\code{obtainSource} and \code{obtainDate} abstracts functions.
}

\seealso{
\code{\link{ExtractorEml}}, \code{\link{ExtractorYtbid}},
\code{\link{Instance}}
}
\section{Super class}{
\code{\link[bdpar:Instance]{bdpar::Instance}} -> \code{ExtractorSms}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ExtractorSms-new}{\code{ExtractorSms$new()}}
\item \href{#method-ExtractorSms-obtainDate}{\code{ExtractorSms$obtainDate()}}
\item \href{#method-ExtractorSms-obtainSource}{\code{ExtractorSms$obtainSource()}}
\item \href{#method-ExtractorSms-toString}{\code{ExtractorSms$toString()}}
\item \href{#method-ExtractorSms-clone}{\code{ExtractorSms$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="addBanPipes"><a href='../../bdpar/html/Instance.html#method-Instance-addBanPipes'><code>bdpar::Instance$addBanPipes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="addFlowPipes"><a href='../../bdpar/html/Instance.html#method-Instance-addFlowPipes'><code>bdpar::Instance$addFlowPipes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="addProperties"><a href='../../bdpar/html/Instance.html#method-Instance-addProperties'><code>bdpar::Instance$addProperties()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="checkCompatibility"><a href='../../bdpar/html/Instance.html#method-Instance-checkCompatibility'><code>bdpar::Instance$checkCompatibility()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="getBanPipes"><a href='../../bdpar/html/Instance.html#method-Instance-getBanPipes'><code>bdpar::Instance$getBanPipes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="getData"><a href='../../bdpar/html/Instance.html#method-Instance-getData'><code>bdpar::Instance$getData()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="getDate"><a href='../../bdpar/html/Instance.html#method-Instance-getDate'><code>bdpar::Instance$getDate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="getFlowPipes"><a href='../../bdpar/html/Instance.html#method-Instance-getFlowPipes'><code>bdpar::Instance$getFlowPipes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="getNamesOfProperties"><a href='../../bdpar/html/Instance.html#method-Instance-getNamesOfProperties'><code>bdpar::Instance$getNamesOfProperties()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="getPath"><a href='../../bdpar/html/Instance.html#method-Instance-getPath'><code>bdpar::Instance$getPath()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="getProperties"><a href='../../bdpar/html/Instance.html#method-Instance-getProperties'><code>bdpar::Instance$getProperties()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="getSource"><a href='../../bdpar/html/Instance.html#method-Instance-getSource'><code>bdpar::Instance$getSource()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="getSpecificProperty"><a href='../../bdpar/html/Instance.html#method-Instance-getSpecificProperty'><code>bdpar::Instance$getSpecificProperty()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="invalidate"><a href='../../bdpar/html/Instance.html#method-Instance-invalidate'><code>bdpar::Instance$invalidate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="isInstanceValid"><a href='../../bdpar/html/Instance.html#method-Instance-isInstanceValid'><code>bdpar::Instance$isInstanceValid()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="isSpecificProperty"><a href='../../bdpar/html/Instance.html#method-Instance-isSpecificProperty'><code>bdpar::Instance$isSpecificProperty()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="setData"><a href='../../bdpar/html/Instance.html#method-Instance-setData'><code>bdpar::Instance$setData()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="setDate"><a href='../../bdpar/html/Instance.html#method-Instance-setDate'><code>bdpar::Instance$setDate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="setProperties"><a href='../../bdpar/html/Instance.html#method-Instance-setProperties'><code>bdpar::Instance$setProperties()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="setSource"><a href='../../bdpar/html/Instance.html#method-Instance-setSource'><code>bdpar::Instance$setSource()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bdpar" data-topic="Instance" data-id="setSpecificProperty"><a href='../../bdpar/html/Instance.html#method-Instance-setSpecificProperty'><code>bdpar::Instance$setSpecificProperty()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ExtractorSms-new"></a>}}
\if{latex}{\out{\hypertarget{method-ExtractorSms-new}{}}}
\subsection{Method \code{new()}}{
Creates a \code{\link{ExtractorSms}} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ExtractorSms$new(path)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{A \code{\link{character}} value. Path of the tsms file.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ExtractorSms-obtainDate"></a>}}
\if{latex}{\out{\hypertarget{method-ExtractorSms-obtainDate}{}}}
\subsection{Method \code{obtainDate()}}{
Obtains the date of the SMS file.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ExtractorSms$obtainDate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ExtractorSms-obtainSource"></a>}}
\if{latex}{\out{\hypertarget{method-ExtractorSms-obtainSource}{}}}
\subsection{Method \code{obtainSource()}}{
Obtains the source of the SMS file. Reads the file indicated
in the path. In addition, it initializes the data field with the initial
source.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ExtractorSms$obtainSource()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ExtractorSms-toString"></a>}}
\if{latex}{\out{\hypertarget{method-ExtractorSms-toString}{}}}
\subsection{Method \code{toString()}}{
Returns a \code{\link{character}} representing the instance
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ExtractorSms$toString()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{\link{Instance}} \code{\link{character}} representation
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ExtractorSms-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ExtractorSms-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ExtractorSms$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
