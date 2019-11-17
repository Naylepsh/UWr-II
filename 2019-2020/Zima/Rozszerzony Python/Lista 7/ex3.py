#!/usr/bin/env python3
# coding: utf8


import html.parser
import urllib.request
import html2text
import threading
import multiprocessing as mp
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

  def __init__(self, starting_url, depth=1):
    self.urls = [starting_url]
    gathering_urls = threading.Thread(target=self._gather_urls, args=(starting_url, depth))
    gathering_urls.start()
    gathering_urls.join()
    self.parser = html2text.HTML2Text()
    self.parser.ignore_links = True
    with mp.Pool() as pool:
      self.pages = list(pool.map(self._count_word_occurences, self.urls))
  
  def _get_occurences(self, page):
    n = page.word_count[self.key] if self.key in page.word_count else 0
    return (page.url, n)

  def __getitem__(self, key):
    self.key = key
    with mp.Pool() as pool:
      res = pool.map(self._get_occurences, self.pages)
    res.sort(key=lambda x: x[1], reverse=True)
    return res
  
  def _gather_urls(self, url, depth):
    if depth > 1:
      parser = URLParser()
      try:
        with urllib.request.urlopen(url) as req:
          parser.feed(req.read().decode('utf-8'))
        for url in parser.urls:
          if url[-1] == '/': url = url[:-1]
          if url not in self.urls:
            self.urls.append(url)
          threading.Thread(target=self._gather_urls, args=(url, depth-1)).start()
      except Exception as e:
        print(f'URL saving error at {url}')
  
  def _count_word_occurences(self, url):
    print(f'getting words from {url}')
    page = Page(url)
    try:
      with urllib.request.urlopen(page.url) as req:
        text = self.parser.handle(req.read().decode('utf-8'))
        for word in text.split(' '):
          page.add_word(word)
      return page
    except Exception as e:
      print(f'skipping {page.url} due to {e}')


if __name__ == '__main__':
  start = time()
  s = GarbageSearch('https://www.ii.uni.wroc.pl/~marcinm/dyd/python/', 2)
  print(time() - start)
  print(s['Python'])
