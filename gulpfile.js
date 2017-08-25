"use strict";

const gulp = require("gulp");
const header = require("gulp-header");
const contentFilter = require("gulp-content-filter");
const rimraf = require("rimraf");
const fs = require("fs");
const trimlines = require("gulp-trimlines");
const replace = require("gulp-replace");
const footer = require("gulp-footer");
const path = require("path");
const exec = require("child_process").exec;
const webpack = require("webpack-stream");
const { injectIconsIntoHTML, createIconPureScript } = require("./script/icons");
const file = require("gulp-file")
const inject = require("gulp-inject")
const packageInfo = require("./package.json")
const rename = require("gulp-rename")
const { sass } = require("./script/sass")
const vinylPaths = require("vinyl-paths")
const del = require("del")

// Coordinate icon attributions through a mutable map because I'm not
// gulp-savvy enough to handle this. --nf
const iconAttributions = {};

const slamDataSources = [
  "src/**/*.purs",
];

function pursBundle(name, main) {
  return function(done) {
    const cmd = [
      'purs bundle "output/**/*.js"',
      '--output', name + '.js',
      '--main', main,
      '--module', main
    ].join(' ');

    exec(cmd, function(stderr, stdout) {
      if (stderr) {
        done(stderr);
      } else {
        done();
      }
    });
  };
}

function loadHeaders() {
  try {
    return fs.readdirSync("./headers").filter(function(name) {
        return /.*\.header/.test(name);
    });
  } catch (e) {
    return [];
  }
}

function webpackConfig(name) {
  return {
    resolve: {
      modulesDirectories: ["node_modules"],
      alias: {
        "package.json": path.join(__dirname, "package.json")
      }
    },
    output: { filename: name + ".js" },
    plugins: [
      new webpack.webpack.optimize.UglifyJsPlugin({
        comments: false,
        compress: {
          unused: true,
          dead_code: true,
          warnings: false,
          drop_debugger: true,
          conditionals: true,
          evaluate: true,
          drop_console: true,
          sequences: true,
          booleans: true,
        },
        sourceMap: false
      }),
    ],
    module: {
      loaders: [
        { include: /\.json$/, loaders: ["json-loader"] },
        { test: require.resolve("echarts"), loader: "expose?echarts" }
      ]
    }
  };
}

// Webdriver thinks elements which are obscured by elements with fixed positions
// are visible. By and large fixed positions fall back gracefully to
// absolute positions so this task can be used to swap the two after less
// compilation.
// This task can be reverted by the `less` task.
gulp.task('prevent-css-transitions-and-remove-fixed-positions', function() {
  var css = fs.readFileSync("test/prevent-css-transitions.css", "utf8");
  gulp.src(['public/css/main.css'])
    .pipe(footer(css))
    .pipe(replace('fixed', 'absolute'))
    .pipe(gulp.dest('public/css'));
});


gulp.task("add-headers", ["replace-headers"], function () {
  // read in the license header
  var licenseHeader = "{-\n" + fs.readFileSync('LICENSE.header', 'utf8') + "-}\n\n";

  // filter out files that already have a license header
  var contentFilterParams = { include: /^(?!\{-\nCopyright)/ };

  // prepend license header to all source files
  return gulp.src(slamDataSources, {base: "./"})
            .pipe(contentFilter(contentFilterParams))
            .pipe(header(licenseHeader))
            .pipe(gulp.dest("./"));
});

/**
* To switch license file
* + Copy old license file to `headers` folder
* + Run `gulp add-headers`
* + Remove `headers` folder
**/
gulp.task("replace-headers", function() {
  var licenseHeader =
    "{-\n" + fs.readFileSync('LICENSE.header', 'utf8') + "-}";

  return loadHeaders()
    .map(function(name) {
      // Read their content
      var file = "./headers/" + name;
      var fileContent;
      try {
        fileContent = "{-\n" + fs.readFileSync(file, "utf8") + "-}";
      } catch (e) {
        // warn here, we don't know maybe we must read this file but it has
        // incorrect permissions
        console.warn(e.message);
        fileContent = undefined;
      }
      return fileContent;
    })
    .filter(function(content) {
      // Filter empty files and files with incorrect permissions
      return typeof content !== "undefined";
    })
    .reduce(function(acc, content) {
      // Foldl gulp task
      return function() {
        return acc().pipe(replace(content, licenseHeader));
      };
    }, function() {
      // Initial gulp task
      return gulp.src(slamDataSources, { base: "./" });
    })()
    .pipe(gulp.dest("./"));
});

gulp.task("replace-crlf", function() {
  gulp.src(slamDataSources, { base: "./" })
    .pipe(replace(/\r\n/g, "\n"))
    .pipe(gulp.dest("./"));
});


gulp.task("trim-whitespace", function () {
  var options = { leading: false };
  return gulp.src(slamDataSources, { base: "./" })
    .pipe(trimlines(options))
    .pipe(gulp.dest("."));
});


var mkBundleTask = function (name, main) {
  gulp.task("prebundle-" + name,
    pursBundle("tmp/" + name, main));

  gulp.task("bundle-" + name, ["prebundle-" + name], function () {
    return gulp.src("tmp/" + name + ".js")
      .pipe(webpack(webpackConfig(name)))
      .pipe(gulp.dest("public/js"));
  });

  return "bundle-" + name;
};

gulp.task("bundle", [
  mkBundleTask("filesystem", "SlamData.FileSystem"),
  mkBundleTask("workspace", "SlamData.Workspace"),
  mkBundleTask("auth_redirect", "SlamData.AuthRedirect"),
]);

gulp.task("bundle-test", ["prevent-css-transitions-and-remove-fixed-positions"],
  pursBundle("test/index", "Test.SlamData.Feature.Main"));

gulp.task("bundle-property-tests",
  pursBundle("tmp/js/property-tests", "Test.SlamData.Property"));

gulp.task("inject-icons", () => injectIconsIntoHTML(iconAttributions));
gulp.task("icons", ["inject-icons"], () => createIconPureScript(iconAttributions));

gulp.task("sass", () => sass());

gulp.task("watch-sass", ["sass"], () => gulp.watch("./sass/**/*.sass", ["sass"]));

gulp.task("full", [ "add-headers", "trim-whitespace" ]);

gulp.task("version-js", () => {
  return gulp.src(["./public/js/workspace.js", "./public/js/filesystem.js"])
    .pipe(vinylPaths(del))
    .pipe(rename((path) => {
      path.basename += "-" + packageInfo.version;
    }))
    .pipe(gulp.dest("./public/js/"));
})

gulp.task("version-files", ["version-js"])

gulp.task("version-inject", () => {
  const workspaceJS = file("workspace-embed", "<script type=\"text/javascript\" src=\"js/workspace-" + packageInfo.version + ".js\"></script>", { src: true })
  const filesystemJS = file("filesystem-embed", "<script type=\"text/javascript\" src=\"js/filesystem-" + packageInfo.version + ".js\"></script>", { src: true })

  return gulp.src("./public/**/*.html")
    .pipe(inject(filesystemJS, {
      starttag: "<!-- filesystem-js -->",
      endtag: "<!-- /filesystem-js -->",
      removeTags: false,
      transform: (filePath, file) => {
        console.log(filePath);
        return file.contents.toString("utf8")
      }
    }))
    .pipe(inject(workspaceJS, {
      starttag: "<!-- workspace-js -->",
      endtag: "<!-- /workspace-js -->",
      removeTags: false,
      transform: (filePath, file) => {
        console.log(filePath);
        return file.contents.toString("utf8")
      }
    }))
    .pipe(gulp.dest('./public'))
    .on("error", () => console.log("what"))
    .on("finish", () => console.log("done"))
})

gulp.task("versionify", ["version-files", "version-inject"])
