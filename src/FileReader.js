// module FileReader


exports.readFileImpl = function readText(file, onSuccess, onFailure) {
  if (file) {
      var reader = new FileReader();
      reader.readAsText(file, "UTF-8");
      reader.onload = onSuccess
      reader.onerror = onFailure
      }
}

exports.filesById = function filesById(document.getElementByk)

exports.files = function files(elem) { elem.files }
exports.firstFile = function files(elem) { elem.files[0] }
//exports.fileReader = function(name) { return new FileReader() }
//exports.readFile = function (name) { 
//    var reader = new FileReader()
//    reader.readAsText(file, "UTF-8");

