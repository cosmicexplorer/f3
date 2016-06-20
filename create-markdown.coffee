#!/usr/bin/env coffee

fs = require 'fs'
{markdown} = require 'markdown'
wordwrap = require 'wordwrap'

processHeader = (node) -> "\n;; #{node[2]}:\n"

processText = (node) ->
  node.replace(/<kbd>(.*?)<\/kbd>/g, (all, g1) -> "'#{g1}'")

processLines = (text) ->
  linesText = wordwrap(77, {mode: 'soft'})(text)
  linesText.split('\n')
    .filter((line) -> line.match(/[^\s]/g)?)
    .map((line) -> ";; #{line}")
    .join('\n')

processPara = (node) ->
  innerText = node[1..].map(processMarkdown).join(' ')
  processLines(innerText) + '\n'

processBulletList = (node) ->
  '\n' + node[1..].map(processBulletItem).join('\n') + '\n'

processBulletItem = (node) ->
  text = node[1..].map(processMarkdown).join(' ')
  processLines "- #{text}"

processList = (node) -> '\n' + node[1..].map(processListItem).join('\n') + '\n'

processListItem = (node, index) ->
  text = node[1..].map(processMarkdown).join(' ')
  processLines "#{index + 1}. #{text}"

processLink = (node) -> node[2]

processInlineCode = (node) -> "'#{node[1]}'"

processMap =
  para: processPara
  header: processHeader
  numberlist: processList
  inlinecode: processInlineCode
  bulletlist: processBulletList
  link: processLink

processMarkdown = (node) ->
  processFun = processMap[node[0]]
  processFun?(node) ? processText(node)

removeExtraComments = (s) ->
  s.replace(/ +$/gm, '')
    .replace(/^((;; )+)/gm, (all, g1) ->
      insertionTextLength = g1.length / 3
      str = new Array(insertionTextLength).join('  ')
      ";; #{str}")
    .replace(/([^; ]) +/g, (all, g1) -> "#{g1} ")
    .replace(/\n(\n)+/g, '\n\n')

licenseNodeFilter = (node) ->
  (not ((node[0] is 'header') and (node[2] is 'License'))) and
    (not ((node[0] is 'para') and (node[1][2] is 'GPL')))

link = "https://github.com/cosmicexplorer/f3"
header = ";; The below is generated from a README at\n;; #{link}.\n"

processTree = (tree) ->
  text = tree[2..].filter(licenseNodeFilter).map(processMarkdown).join('\n')
  removeExtraComments(text).replace(/\n+$/g, '')

readme = fs.readFileSync("#{__dirname}/README.md").toString()
f3El = fs.readFileSync("#{__dirname}/f3.el").toString()

output = f3El.replace(
  /(;;; Commentary:)\n(;; End Commentary)/g, (all, g1, g2) ->
    tree = markdown.parse readme
    "#{g1}\n\n#{header}\n#{processTree(tree)}\n\n#{g2}")

fs.writeFileSync "#{__dirname}/f3.el", output
