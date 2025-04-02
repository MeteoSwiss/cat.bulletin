\NeedsTeXFormat{LaTeX2e}[2001/06/01]
\ProvidesClass{mch_basisformular}[2015/03/02 v1 by hir]

%% Article options
\DeclareOption*{
  \PassOptionsToClass{\CurrentOption}{article}
}
\ProcessOptions
\LoadClass[a4paper]{article}

\RequirePackage{graphicx} % allow including images

\RequirePackage[top=48mm,bottom=16mm,left=30mm,right=20mm]{geometry} % set margins of the document

\RequirePackage[T1]{fontenc} %define font encoding
%\RequirePackage[utf8]{inputenc} %define file encoding
\renewcommand\familydefault{phv} %define font family

% define character size of titles and subtitles
\RequirePackage{sectsty}
%\renewcommand\title[1]{\section*{\fontsize{21}{25}\selectfont#1}}
\sectionfont{\fontsize{16}{19}\selectfont}
\subsectionfont{\fontsize{14}{17}\selectfont}
\subsubsectionfont{\fontsize{12}{15}\selectfont}

%\RequirePackage{frutiger}
\RequirePackage{lastpage} % needed for page numbering

\RequirePackage[ddmmyyyy, 24hr]{datetime} % modify default format of \today and \currenttime
\renewcommand\dateseparator{.}

\RequirePackage[absolute]{textpos} % define text boxes with absolute positions as needed in header and footer
\RequirePackage{setspace} % allows to modify spacing


% Define frontpage style
\def\@defaultfooter{
 \vbox{
   {\fontsize{8}{9}\selectfont
    % \begin{singlespace}
      \begin{flushleft}
        \begin{textblock*}{100mm}[0,1](30mm,291mm)
          \copyright@meteo \newline \@contact
        \end{textblock*}
      \end{flushleft}
      \begin{flushright}
        \begin{textblock*}{60mm}[1,1](190mm,291mm)
          \cat@package
        \end{textblock*}
      \end{flushright}
    % \end{singlespace}
    }
  }
}

\def\@frontpage{%
  \vbox{
    \begin{singlespace}
      \begin{flushleft}
        \begin{textblock*}{224mm}(21.75mm,10.879mm)
          \includegraphics{\header@logo}
        \end{textblock*}
      \end{flushleft}
    \end{singlespace}
  }
}

% Define style for all other pages
\def\@standard{%
  \vbox{
    \begin{singlespace}
      \begin{flushleft}
        \begin{textblock*}{50mm}(30mm,12mm)
          \header@left
        \end{textblock*}
        \begin{textblock*}{10mm}[0,1](197mm,281mm)
          \thepage/\pageref{LastPage}
        \end{textblock*}
      \end{flushleft}
      \begin{center}
        \begin{textblock*}{50mm}[0.5,0](110mm,12mm)
          \header@center
        \end{textblock*}
      \end{center}
      \begin{flushright}
        \begin{textblock*}{50mm}[1,0](190mm,12mm)
          \header@right
        \end{textblock*}
      \end{flushright}
    \end{singlespace}
  }
}

% Define page layouts
\def\ps@standard{
  \let\@mkboth\@gobbletwo
  \let\@oddhead\@standard\let\@oddfoot\@defaultfooter
  \let\@evenhead\@standard\let\@evenfoot\@defaultfooter
}

\def\ps@frontpage{
  \let\@mkboth\@gobbletwo
  \let\@oddhead\@frontpage\let\@oddfoot\@defaultfooter
  \let\@evenhead\@frontpage\let\@evenfoot\@defaultfooter
}

% Define vaiables to set header footer etc. within document
\newcommand{\catpackage}[1]{\def\cat@package{#1, \today\ \currenttime}}
\newcommand{\copyrightmeteo}[1]{\def\copyright@meteo{#1}}
\newcommand{\contact}[1]{\def\@contact{#1}}
\newcommand{\headerleft}[1]{\def\header@left{#1}}
\newcommand{\headercenter}[1]{\def\header@center{#1}}
\newcommand{\headerright}[1]{\def\header@right{#1}}
\newcommand{\lang}[1]{\def\header@logo{logo_#1.pdf}}

% Set defaults
\catpackage{undefined tool} % cat name displayed in footer
\copyrightmeteo{\textcopyright\hspace{0.3em}MeteoSwiss} % copyright displayed in footer
\contact{Contact: customerservice@meteoswiss.ch} % contact information displayed in footer

\headerleft{\empty} % left header displayed on page 2ff
\headercenter{\empty} % center header displayed on page 2ff
\headerright{\empty} % right header displayed on page 2ff

\lang{E} % document language to select the corresponding logo, all other information needs translation within the document

\pagestyle{standard} % set pagestyle, frontpage needs to be activated within the document for the first page

\setlength{\parindent}{0pt} % CD Bund does not define a paragraph indent
