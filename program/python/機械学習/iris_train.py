from sklearn import svm, metrics
import random
import re

csv = []
with open('iris.csv', 'r', encoding='utf-8') as fp:
    for line in fp:
        line = line.strip()
        cols = line.split(',')

        def fn(n): return float(n) if re.match(r'^[0-9\.]+$', n) else n
        cols = list(map(fn, cols))
        csv.append(cols)

del csv[0]

random.shuffle(csv)  # shuffle

total_len = len(csv)
train_len = int(len(csv) * 2 / 3)

train_data = []
train_label = []
test_data = []
test_label = []

for i in range(total_len):
    data = csv[i][0:4]
    label = csv[i][4]

    if i < train_len:
        train_data.append(data)
        train_label.append(label)

    else:
        test_data.append(data)
        test_label.append(label)

clf = svm.SVC()
clf.fit(train_data, train_label)
pre = clf.predict(test_data)

ac_score = metrics.accuracy_score(test_label, pre)
print('正解率:', ac_score)
