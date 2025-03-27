# NBA Emailer

This goes to https://lacronicadesdeelsofa.com/sofialert/, grabs the spoiler-free ratings for games on that site and uses MailerSend to send the results to users in the RECIPIENT_EMAIL environment variable. 

It is hooked up with a github workflow to run on a cronjob.

It is "vibe-coded" in OCaml, because, why not?
