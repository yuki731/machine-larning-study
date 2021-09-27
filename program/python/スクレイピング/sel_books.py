from bs4 import BeautifulSoup
fp = open('books.html', encoding='utf-8')
soup = BeautifulSoup(fp, 'html.parser')


def sel(q): return print(soup.select_one(q).string)


sel('#nu')
sel('li#nu')
sel('ul > li#nu')
sel('#bible > #nu')
