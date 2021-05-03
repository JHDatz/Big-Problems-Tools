# If running in Jupyter Notebook, you'll need to make changes in Jupyter's config file by using the following link:
# https://stackoverflow.com/questions/43288550/iopub-data-rate-exceeded-in-jupyter-notebook-when-viewing-image
#
# I also found that the following installation steps were necessary:
#
# 1. Installation of the Connector/Python Module at this link: https://dev.mysql.com/downloads/connector/python/
# 2. Installation of the package mysql-connector-python-rf

import mysql.connector

conn = mysql.connector.connect(user = 'redacted', password = 'redacted',
                               host = 'redacted',
                               port = 3306,
                               database = 'lahman')

cur = conn.cursor()
cur.execute("blah blah blah")
print(cur.fetchall())
conn.close()

# For interacting with Pandas:

print(pd.read_sql("blah blah blah", conn))
conn.close()

