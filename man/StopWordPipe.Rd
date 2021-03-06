% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StopWordPipe.R
\name{StopWordPipe}
\alias{StopWordPipe}
\title{Class to find and/or remove the stop words on the data field of an Instance}
\description{
\code{\link{StopWordPipe}} class is responsible for detecting
the existing stop words in the \strong{data} field of each \code{\link{Instance}}.
Identified stop words are stored inside the \strong{contraction} field of
\code{\link{Instance}} class. Moreover if needed, is able to perform inline
stop words removement.
}
\section{Details}{

\code{\link{StopWordPipe}} class requires the resource files (in json format)
containing the list of stop words. To this end, the language of the text
indicated in the \emph{propertyLanguageName} should be contained in the
resource file name (ie. xxx.json where xxx is the value defined in the
\emph{propertyLanguageName} ). The location of the resources should be
defined in the \strong{"resources.stopwords.path"} field of
\emph{\link{bdpar.Options}} variable.
}

\section{Note}{

\code{\link{StopWordPipe}} will automatically invalidate the
\code{\link{Instance}} whenever the obtained data is empty.
}

\section{Inherit}{

This class inherits from \code{\link{GenericPipe}} and implements the
\code{pipe} abstract function.
}

\seealso{
\code{\link{AbbreviationPipe}}, \code{\link{bdpar.Options}},
         \code{\link{ContractionPipe}}, \code{\link{File2Pipe}},
         \code{\link{FindEmojiPipe}}, \code{\link{FindEmoticonPipe}},
         \code{\link{FindHashtagPipe}}, \code{\link{FindUrlPipe}},
         \code{\link{FindUserNamePipe}}, \code{\link{GuessDatePipe}},
         \code{\link{GuessLanguagePipe}}, \code{\link{Instance}},
         \code{\link{InterjectionPipe}}, \code{\link{MeasureLengthPipe}},
         \code{\link{GenericPipe}}, \code{\link{ResourceHandler}},
         \code{\link{SlangPipe}}, \code{\link{StoreFileExtPipe}},
         \code{\link{TargetAssigningPipe}}, \code{\link{TeeCSVPipe}},
         \code{\link{ToLowerCasePipe}}
}
\section{Super class}{
\code{\link[bdpar:GenericPipe]{bdpar::GenericPipe}} -> \code{StopWordPipe}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{StopWordPipe$new()}}
\item \href{#method-pipe}{\code{StopWordPipe$pipe()}}
\item \href{#method-findStopWord}{\code{StopWordPipe$findStopWord()}}
\item \href{#method-removeStopWord}{\code{StopWordPipe$removeStopWord()}}
\item \href{#method-getPropertyLanguageName}{\code{StopWordPipe$getPropertyLanguageName()}}
\item \href{#method-getResourcesStopWordsPath}{\code{StopWordPipe$getResourcesStopWordsPath()}}
\item \href{#method-setResourcesStopWordsPath}{\code{StopWordPipe$setResourcesStopWordsPath()}}
\item \href{#method-clone}{\code{StopWordPipe$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="bdpar" data-topic="GenericPipe" data-id="getAlwaysBeforeDeps">}\href{../../bdpar/html/GenericPipe.html#method-getAlwaysBeforeDeps}{\code{bdpar::GenericPipe$getAlwaysBeforeDeps()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="bdpar" data-topic="GenericPipe" data-id="getNotAfterDeps">}\href{../../bdpar/html/GenericPipe.html#method-getNotAfterDeps}{\code{bdpar::GenericPipe$getNotAfterDeps()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="bdpar" data-topic="GenericPipe" data-id="getPropertyName">}\href{../../bdpar/html/GenericPipe.html#method-getPropertyName}{\code{bdpar::GenericPipe$getPropertyName()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="bdpar" data-topic="GenericPipe" data-id="hash">}\href{../../bdpar/html/GenericPipe.html#method-hash}{\code{bdpar::GenericPipe$hash()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="bdpar" data-topic="GenericPipe" data-id="setAlwaysBeforeDeps">}\href{../../bdpar/html/GenericPipe.html#method-setAlwaysBeforeDeps}{\code{bdpar::GenericPipe$setAlwaysBeforeDeps()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="bdpar" data-topic="GenericPipe" data-id="setNotAfterDeps">}\href{../../bdpar/html/GenericPipe.html#method-setNotAfterDeps}{\code{bdpar::GenericPipe$setNotAfterDeps()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="bdpar" data-topic="GenericPipe" data-id="setPropertyName">}\href{../../bdpar/html/GenericPipe.html#method-setPropertyName}{\code{bdpar::GenericPipe$setPropertyName()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Creates a \code{\link{StopWordPipe}} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StopWordPipe$new(
  propertyName = "stopWord",
  propertyLanguageName = "language",
  alwaysBeforeDeps = list("GuessLanguagePipe"),
  notAfterDeps = list("AbbreviationPipe"),
  removeStopWords = TRUE,
  resourcesStopWordsPath = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{propertyName}}{A \code{\link{character}} value. Name of the property
associated with the \code{\link{GenericPipe}}.}

\item{\code{propertyLanguageName}}{A \code{\link{character}} value. Name of the
language property.}

\item{\code{alwaysBeforeDeps}}{A \code{\link{list}} value. The dependencies
alwaysBefore (\code{\link{GenericPipe}s} that must be executed before
this one).}

\item{\code{notAfterDeps}}{A \code{\link{list}} value. The dependencies
notAfter (\code{\link{GenericPipe}s} that cannot be executed after
this one).}

\item{\code{removeStopWords}}{A \code{\link{logical}} value. Indicates if
the stop words are removed or not.}

\item{\code{resourcesStopWordsPath}}{A \code{\link{character}} value. Path
of resource files (in json format) containing the stop words.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-pipe"></a>}}
\if{latex}{\out{\hypertarget{method-pipe}{}}}
\subsection{Method \code{pipe()}}{
Preprocesses the \code{\link{Instance}} to obtain/remove
the stop words. The stop words found in the data are added to the
list of properties of the \code{\link{Instance}}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StopWordPipe$pipe(instance)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{instance}}{A \code{\link{Instance}} value. The \code{\link{Instance}}
to preprocess.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The \code{\link{Instance}} with the modifications that have
occurred in the pipe.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-findStopWord"></a>}}
\if{latex}{\out{\hypertarget{method-findStopWord}{}}}
\subsection{Method \code{findStopWord()}}{
Checks if the stop word is in the data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StopWordPipe$findStopWord(data, stopWord)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{A \code{\link{character}} value. The text where stop word
will be searched.}

\item{\code{stopWord}}{A \code{\link{character}} value. Indicates the
stop word to find.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{\link{logical}} value depending on whether the
stop word is in the data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-removeStopWord"></a>}}
\if{latex}{\out{\hypertarget{method-removeStopWord}{}}}
\subsection{Method \code{removeStopWord()}}{
Removes the \emph{stop word} in the data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StopWordPipe$removeStopWord(stopWord, data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{stopWord}}{A \code{\link{character}} value. Indicates the
stop word to remove.}

\item{\code{data}}{A \code{\link{character}} value. The text where stop word
will be removed.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The data with the stop words removed.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getPropertyLanguageName"></a>}}
\if{latex}{\out{\hypertarget{method-getPropertyLanguageName}{}}}
\subsection{Method \code{getPropertyLanguageName()}}{
Gets the name of property language.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StopWordPipe$getPropertyLanguageName()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Value of name of property language.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getResourcesStopWordsPath"></a>}}
\if{latex}{\out{\hypertarget{method-getResourcesStopWordsPath}{}}}
\subsection{Method \code{getResourcesStopWordsPath()}}{
Gets the path of stop words resources.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StopWordPipe$getResourcesStopWordsPath()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Value of path of stop words resources.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-setResourcesStopWordsPath"></a>}}
\if{latex}{\out{\hypertarget{method-setResourcesStopWordsPath}{}}}
\subsection{Method \code{setResourcesStopWordsPath()}}{
Sets the path of stop words resources.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StopWordPipe$setResourcesStopWordsPath(path)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{A \code{\link{character}} value. The new value of the path of
stop words resources.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StopWordPipe$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
