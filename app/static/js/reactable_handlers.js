Shiny.addCustomMessageHandler('highlightRow', function(data) {
  console.log("reactable_handlers.js loaded");

  console.log("Received row index: ", data.rowIndex);  // Pro kontrolu, zda data dorazí
  var rows = document.querySelectorAll('.reactable-row');
  console.log("Number of rows found: ", rows.length);  // Zkontrolujte počet řádků
  if (rows.length > data.rowIndex) {
    rows[data.rowIndex].classList.add('selected');
    console.log("Row highlighted");
  }
});
