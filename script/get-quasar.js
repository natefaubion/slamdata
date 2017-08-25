var https = require("https");
var fs = require("fs");
var cp = require("child_process");

const JAR_LOCATION = "./jars/quasar.jar";
const CONFIG_FILE = "../quasar-versions.json";

var quasar = require(CONFIG_FILE);

function githubRequestHeaders(options) {
  var requestHeaders =
    { "User-Agent": "GitHubAPI"
    , "Authorization": "token " + options.authorizationToken
    };

  return requestHeaders;
};

function fetchReleaseInfo(options, k) {
  var requestOptions =
    { host: "api.github.com"
    , headers: githubRequestHeaders(options)
    , path: "/repos/" + options.owner + "/" + options.repo + "/releases/tags/" + options.tag
    };

  https.get(requestOptions, function (response) {
    var body = "";

    response.on("data", function (chunk) {
      body += chunk;
    });

    response.on("end", function () {
      k(JSON.parse(body));
    });
  });
}

function fetchReleaseAsset(options, destFile, k) {
  var requestHeaders = githubRequestHeaders(options);
  requestHeaders["Accept"] = "application/octet-stream";

  var requestOptions =
    { host: "api.github.com"
    , headers: requestHeaders
    , path: "/repos/" + options.owner + "/" + options.repo + "/releases/assets/" + options.assetId
    };

  https.get(requestOptions, function (response) {
    var assetLocation = response.headers.location;
    if (assetLocation == undefined) {
      throw "Asset location not found";
    };
    https.get(assetLocation, function (assetResponse) {
      if (process.stdout.isTTY) {
        var contentLength = parseInt(assetResponse.headers['content-length'], 10);
        var downloaded = 0;
        var lastUpdate = 0;
        assetResponse.on("data", function (chunk) {
          downloaded += chunk.length;
          var percent = Math.round(downloaded / contentLength * 10000) / 100;
          var now = Date.now();
          if (now - lastUpdate > 500) {
            process.stdout.clearLine();
            process.stdout.cursorTo(0);
            process.stdout.write("Downloading... " + percent + "%");
            lastUpdate = now;
          }
        });
        assetResponse.on("end", function (chunk) {
          process.stdout.clearLine();
          process.stdout.cursorTo(0);
        });
      }
      assetResponse.pipe(destFile);
      destFile.on("finish", k);
    });
  });
};

function downloadQuasar(quasarOptions, token, destFile, k) {
  quasarOptions.authorizationToken = token;

  fetchReleaseInfo(quasarOptions, function (info) {
    var assets =
      info.assets.filter(function (asset) {
        return asset.name.indexOf(quasarOptions.prefix) !== -1
      });

    var asset = assets[0];
    if (asset == undefined) {
      throw "No JAR asset found";
    }

    var assetOptions =
      { owner: quasarOptions.owner
      , repo: quasarOptions.repo
      , assetId: asset.id
      , authorizationToken: token
      };

    fetchReleaseAsset(assetOptions, destFile, k);
  });
}

function checkQuasarVersion(quasarDistribution) {
  return new Promise((resolve, reject) => {
    if (fs.existsSync(JAR_LOCATION)) {
      var proc = cp.spawn("java", ("-jar " + JAR_LOCATION + " --help").split(" "));

      var version = quasar[quasarDistribution].tag.replace(/v?([0-9\.]+)/, "$1");

      var out = [];
      proc.stdout.on('data', function(d){ out.push(d); });
      proc.stdout.on('end', function(){
        var buf = Buffer.concat(out);
        resolve(buf.toString().indexOf("quasar " + version) != -1);
      });

      var err = [];
      proc.stderr.on('data', function(d){ err.push(d); });
      proc.stderr.on('end', function(){
        reject(new Error(Buffer.concat(err).toString()));
      });
    } else {
      resolve(false)
    }
  });
}

function fetchQuasar(githubAuthToken, quasarDistribution) {
  return new Promise ((resolve, reject) => {
    if (githubAuthToken == undefined) {
      reject(new Error("Quasar jar will not be fetched unless you set the GITHUB_AUTH_TOKEN environment variable"));
    } else {
      var start = Date.now();
      console.log("Downloading " + quasarDistribution + " @ " + quasar[quasarDistribution].tag + " ...");
      var quasarDest = fs.createWriteStream(JAR_LOCATION);
      downloadQuasar(quasar[quasarDistribution], githubAuthToken, quasarDest, function () {
        quasarDest.close(function () {
          var elapsed = Math.round((Date.now() - start) / 1000);
          console.log("Downloaded in " + elapsed + "s");
          resolve("Downloaded new JAR");
        });
      });
    }
  });
}

function main() {
  var githubAuthToken = process.env.GITHUB_AUTH_TOKEN

  var quasarDistribution = process.argv.indexOf("--advanced") != -1 ? "slamdata-backend" : "quasar";

  if (!fs.existsSync("./jars")){
    fs.mkdirSync("./jars");
  }

  checkQuasarVersion(quasarDistribution)
    .then(isOk =>
      isOk === true
        ? "Existing JAR is up to date for " + quasarDistribution + " " + quasar[quasarDistribution].tag
        : fetchQuasar(githubAuthToken, quasarDistribution))
    .catch(err => {
      console.log("An update JAR will be fetched - an error occurred when checking the existing version:\n\n\t" + err.message.replace(/\n/, "\n\t") + "\n");
      return fetchQuasar(githubAuthToken, quasarDistribution)
    })
    .catch(err => console.error("Failed to fetch quasar", err))
    .then(msg => console.log("Done! (" + msg + ")"));
}

main();
