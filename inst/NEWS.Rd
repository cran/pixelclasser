\name{NEWS}
\title{pixelclasser News}
\encoding{UTF-8}

\section{Changes in version 1.1.1}{
  \itemize{
    \item Using _PACKAGE in pixelclasser-package.R to obtain correct access to
    the package overview file. Required after changes in roxygen2 behaviour.
  }
}

\section{Changes in version 1.1.0}{
    
  \subsection{NEW FEATURES}{
    \itemize{
      \item The class names of all objects created by functions in pixelclasser
      now start with \code{pixel_}. The internal structure did not change, however,
      so it is safe to change the class of any object created with version 1.0
      into the new classes with \code{class(my_object) <- "new_class_name"}.
      
      \item Functions \code{is_()} and \code{summary()} (S3) were added for the
      classes created by \code{pixelclasser}.
    }
  }
  
  \subsection{DEPRECATED}{
    \itemize{

      \item Functions \code{create_rule()}, \code{create_subclass()} and 
      \code{create_class()} were replaced by \code{pixel_rule()},
      \code{pixel_subclass()} and \code{pixel_class()}, to make the name of the 
      object constructors equal to the class name.

    }  
    
  } % end DEPRECATED
}
