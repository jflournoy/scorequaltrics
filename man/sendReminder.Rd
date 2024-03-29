% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panels.R
\name{sendReminder}
\alias{sendReminder}
\title{Sends a survey to the given recipient.}
\usage{
sendReminder(
  username,
  password,
  parentEmailDistributionId,
  sendDate,
  fromEmail,
  fromName,
  subject,
  messageId,
  libraryId
)
}
\arguments{
\item{username}{the Qualtrics username.}

\item{password}{the Qualtrics password.}

\item{parentEmailDistributionId}{see Qualtrics documentation.}

\item{sendDate}{the date to send the invitation.}

\item{fromEmail}{the email address of the sender.}

\item{fromName}{the name of the sender.}

\item{subject}{the subject of the email to send.}

\item{messageId}{the Qualtrics message id from the library.}

\item{libraryId}{the Qualtrics library id containing the message to send.}
}
\description{
Sends a survey to the given recipient.
}
