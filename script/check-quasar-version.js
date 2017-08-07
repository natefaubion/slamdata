const fs = require("fs")

const quasarFilePath = 'quasar-versions.json'
const dockerFilePath = 'docker/Dockerfiles/Slamdata/Slamdata-Dockerfile'

fs.readFile(dockerFilePath, 'utf8', function(err, dataQuasar) {
  if (err) {
    return console.log(err);
  }

  fs.readFile(quasarFilePath, (err, data) => {
    if (err) {
      return console.log(err);
    };
    const obj = JSON.parse(data);
    const quasarVer = obj.quasar.tag.replace('v', '')
    const reg = /\d+\.\d+\.\d+/
    const qVjson = dataQuasar.match(reg)

    if (qVjson[0] !== quasarVer) {
      throw new Error('Your quasar versions do not match on ' + quasarFilePath + ' -> ' + dockerFilePath + '\n' +
                      'run `npm run update:quasar`')
    }
  });
});
