"use strict"

const autoprefixer = require("gulp-autoprefixer")
const cleanCSS = require("gulp-clean-css")
const gulp = require("gulp")
const rename = require("gulp-rename")
const sass = require("gulp-sass")
const { version } = require("../package.json")


const autoprefixerOpts = {
  browsers: "> 2%, last 1 versions, Firefox ESR, not Explorer < 11, not Android < 4.4"
}


const cleanCSSOpts = {
  advanced: true,
  level: 2
}


const sass_ = () =>
  gulp.src("./sass/themes/*.sass")
    .pipe(sass())
    .pipe(autoprefixer(autoprefixerOpts))
    .pipe(cleanCSS(cleanCSSOpts))
    .pipe(rename(function(path) { path.basename += "-" + version }))
    .pipe(gulp.dest("public/css/"))


const pageOverlaySass = () =>
  gulp.src("./sass/page-loading-overlay.sass")
    .pipe(sass())
    .pipe(autoprefixer(autoprefixerOpts))
    .pipe(cleanCSS(cleanCSSOpts))


module.exports = {
  sass: sass_,
  pageOverlaySass
}
