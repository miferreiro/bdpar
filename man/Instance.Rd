% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Instance.R
\name{Instance}
\alias{Instance}
\title{Abstract super class that handles the management of the Instances}
\description{
Provides the required methods to successfully handle each
\code{\link{Instance}} class.
}
\seealso{
\code{\link{ExtractorEml}}, \code{\link{ExtractorSms}},
\code{\link{ExtractorYtbid}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Instance-new}{\code{Instance$new()}}
\item \href{#method-Instance-obtainDate}{\code{Instance$obtainDate()}}
\item \href{#method-Instance-obtainSource}{\code{Instance$obtainSource()}}
\item \href{#method-Instance-getDate}{\code{Instance$getDate()}}
\item \href{#method-Instance-getSource}{\code{Instance$getSource()}}
\item \href{#method-Instance-getPath}{\code{Instance$getPath()}}
\item \href{#method-Instance-getData}{\code{Instance$getData()}}
\item \href{#method-Instance-getProperties}{\code{Instance$getProperties()}}
\item \href{#method-Instance-setSource}{\code{Instance$setSource()}}
\item \href{#method-Instance-setData}{\code{Instance$setData()}}
\item \href{#method-Instance-setDate}{\code{Instance$setDate()}}
\item \href{#method-Instance-setProperties}{\code{Instance$setProperties()}}
\item \href{#method-Instance-addProperties}{\code{Instance$addProperties()}}
\item \href{#method-Instance-getSpecificProperty}{\code{Instance$getSpecificProperty()}}
\item \href{#method-Instance-isSpecificProperty}{\code{Instance$isSpecificProperty()}}
\item \href{#method-Instance-setSpecificProperty}{\code{Instance$setSpecificProperty()}}
\item \href{#method-Instance-getNamesOfProperties}{\code{Instance$getNamesOfProperties()}}
\item \href{#method-Instance-isInstanceValid}{\code{Instance$isInstanceValid()}}
\item \href{#method-Instance-invalidate}{\code{Instance$invalidate()}}
\item \href{#method-Instance-getFlowPipes}{\code{Instance$getFlowPipes()}}
\item \href{#method-Instance-addFlowPipes}{\code{Instance$addFlowPipes()}}
\item \href{#method-Instance-getBanPipes}{\code{Instance$getBanPipes()}}
\item \href{#method-Instance-addBanPipes}{\code{Instance$addBanPipes()}}
\item \href{#method-Instance-checkCompatibility}{\code{Instance$checkCompatibility()}}
\item \href{#method-Instance-toString}{\code{Instance$toString()}}
\item \href{#method-Instance-clone}{\code{Instance$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-new"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-new}{}}}
\subsection{Method \code{new()}}{
Creates a \code{\link{Instance}} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$new(path)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{A \code{\link{character}} value. Path of the file.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-obtainDate"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-obtainDate}{}}}
\subsection{Method \code{obtainDate()}}{
Abstract function responsible for obtaining the date of the
\code{\link{Instance}}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$obtainDate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-obtainSource"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-obtainSource}{}}}
\subsection{Method \code{obtainSource()}}{
Abstract function responsible for determining the source of
the \code{\link{Instance}}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$obtainSource()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-getDate"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-getDate}{}}}
\subsection{Method \code{getDate()}}{
Gets the date.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$getDate()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Value of date.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-getSource"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-getSource}{}}}
\subsection{Method \code{getSource()}}{
Gets the source.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$getSource()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Value of source.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-getPath"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-getPath}{}}}
\subsection{Method \code{getPath()}}{
Gets the path.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$getPath()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Value of path.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-getData"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-getData}{}}}
\subsection{Method \code{getData()}}{
Gets the data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$getData()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Value of data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-getProperties"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-getProperties}{}}}
\subsection{Method \code{getProperties()}}{
Gets the properties
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$getProperties()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Value of properties.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-setSource"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-setSource}{}}}
\subsection{Method \code{setSource()}}{
Modifies the source value.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$setSource(source)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{source}}{A \code{\link{character}} value. The new value of source.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-setData"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-setData}{}}}
\subsection{Method \code{setData()}}{
Modifies the data value.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$setData(data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{A \code{\link{character}} value. The new value of data.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-setDate"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-setDate}{}}}
\subsection{Method \code{setDate()}}{
Modifies the date value.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$setDate(date)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{date}}{A \code{\link{character}} value. The new value of date.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-setProperties"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-setProperties}{}}}
\subsection{Method \code{setProperties()}}{
Modifies the properties value.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$setProperties(properties)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{properties}}{A \code{\link{list}} value. The new list of properties.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-addProperties"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-addProperties}{}}}
\subsection{Method \code{addProperties()}}{
Adds a property to the list of the properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$addProperties(propertyValue, propertyName)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{propertyValue}}{A \code{Object} value. The value of the new property.}

\item{\code{propertyName}}{A \code{\link{character}} value. The name of the new
property.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-getSpecificProperty"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-getSpecificProperty}{}}}
\subsection{Method \code{getSpecificProperty()}}{
Obtains a specific property.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$getSpecificProperty(propertyName)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{propertyName}}{A \code{\link{character}} value. The name of the
property to obtain.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The value of the specific property.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-isSpecificProperty"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-isSpecificProperty}{}}}
\subsection{Method \code{isSpecificProperty()}}{
Checks for the existence of an specific property.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$isSpecificProperty(propertyName)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{propertyName}}{A \code{\link{character}} value. The name of the
property to check.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A logical results according to the existence of the specific
property in the list of properties.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-setSpecificProperty"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-setSpecificProperty}{}}}
\subsection{Method \code{setSpecificProperty()}}{
Modifies the value of the one property.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$setSpecificProperty(propertyName, propertyValue)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{propertyName}}{A \code{\link{character}} value. The name of the
property.}

\item{\code{propertyValue}}{A \code{Object} value. The new value of the property.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-getNamesOfProperties"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-getNamesOfProperties}{}}}
\subsection{Method \code{getNamesOfProperties()}}{
Gets of the names of all properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$getNamesOfProperties()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The names of properties.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-isInstanceValid"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-isInstanceValid}{}}}
\subsection{Method \code{isInstanceValid()}}{
Checks if the \code{\link{Instance}} is valid.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$isInstanceValid()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Value of isValid flag.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-invalidate"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-invalidate}{}}}
\subsection{Method \code{invalidate()}}{
Forces the invalidation of an specific \code{\link{Instance}}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$invalidate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-getFlowPipes"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-getFlowPipes}{}}}
\subsection{Method \code{getFlowPipes()}}{
Gets the list of the flow of \code{\link{GenericPipe}}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$getFlowPipes()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Names of the \code{\link{GenericPipe}} used.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-addFlowPipes"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-addFlowPipes}{}}}
\subsection{Method \code{addFlowPipes()}}{
Gets the list of the flow of \code{\link{GenericPipe}}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$addFlowPipes(namePipe)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{namePipe}}{A \code{\link{character}} value. Name of the new
\code{\link{GenericPipe}} to be added in the \code{\link{GenericPipeline}}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-getBanPipes"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-getBanPipes}{}}}
\subsection{Method \code{getBanPipes()}}{
Gets an array with containing all the ban
\code{\link{GenericPipe}}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$getBanPipes()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Value of ban \code{\link{GenericPipe}} array.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-addBanPipes"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-addBanPipes}{}}}
\subsection{Method \code{addBanPipes()}}{
Added the name of the Pipe to the array that keeps the track
of \code{\link{GenericPipe}s} having running after restrictions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$addBanPipes(namePipe)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{namePipe}}{A \code{\link{character}} value.
\code{\link{GenericPipe}} name to be introduced into the ban array.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-checkCompatibility"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-checkCompatibility}{}}}
\subsection{Method \code{checkCompatibility()}}{
Check compatibility between \code{\link{GenericPipe}s}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$checkCompatibility(namePipe, alwaysBefore)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{namePipe}}{A \code{\link{character}} value. The name of the
\code{\link{GenericPipe}} name to check the compatibility.}

\item{\code{alwaysBefore}}{A \code{\link{list}} value.
\code{\link{GenericPipe}s} that the \code{\link{Instance}} had to go
through.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-toString"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-toString}{}}}
\subsection{Method \code{toString()}}{
Returns a \code{\link{character}} representing the instance
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$toString()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{\link{Instance}} \code{\link{character}} representation
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Instance-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Instance-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Instance$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
