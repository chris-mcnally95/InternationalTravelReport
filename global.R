# Issue Connection Stop --- This should be moved to the bottom of global.R
shiny::onStop(function(){dbDisconnect(con)})