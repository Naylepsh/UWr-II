#!/usr/bin/env python3
# coding: utf8


import html.parser
import urllib.request
import html2text
from time import time


class URLParser(html.parser.HTMLParser):

  def __init__(self):
    super().__init__()
    self.urls = []
  
  def _is_valid_url(self, string):
    return string.startswith('http')
  
  def handle_starttag(self, tag, attrs):
    if tag == 'a':
      for atr, val in attrs:
        if atr == 'href' and self._is_valid_url(val):
          self.urls.append(val)


class Page():

  def __init__(self, url):
    self.url = url
    self.word_count = {}
  
  def add_word(self, word):
    if word not in self.word_count:
      self.word_count[word] = 0
    self.word_count[word] += 1

class GarbageSearch():

  def __init__(self, starting_URL, depth=1):
    self.pages = [Page(starting_URL)]
    self._gather_URLS(starting_URL, depth)
    self.parser = html2text.HTML2Text()
    self.parser.ignore_links = True
    for page in self.pages:
      self._count_word_occurences(page)
  
  def __getitem__(self, key):
    res = []
    for page in self.pages:
      n = page.word_count[key] if key in page.word_count else 0
      if n:
        res.append((page.url, n))
    res.sort(key=lambda x: x[1], reverse=True)
    return res
  
  def _gather_URLS(self, url, depth):
    if depth > 1:
      parser = URLParser()
      with urllib.request.urlopen(url) as page:
        parser.feed(page.read().decode('utf-8'))
      for url in parser.urls:
        if url[-1] == '/': url = url[:-1]
        if url not in map(lambda page: page.url, self.pages):
          self.pages.append(Page(url))
        self._gather_URLS(url, depth-1)
  
  def _count_word_occurences(self, page):
    with urllib.request.urlopen(page.url) as req:
      text = self.parser.handle(req.read().decode('utf-8'))
      for word in text.split(' '):
        page.add_word(word)


start = time()
s = GarbageSearch('https://www.ii.uni.wroc.pl/~marcinm/dyd/python/', 2)
print(time() - start)
print(s['Python'])
