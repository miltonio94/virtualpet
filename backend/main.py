from flask import Flask, request, render_template, jsonify, send_from_directory
from pymongo import MongoClient
import datetime
import os
import credentials.credentials as cred

# setup
ON_HEROKU = "ON_HEROKU" in os.environ

uri = "mongodb://%s:%s@%s/%s" % (cred.user, cred.password, cred.host, cred.db_name)
client = MongoClient(uri)
db = client[cred.db_name]

# helper functions

def inserInstructionSet(instructionSet):
    print(uri)
    collection = db.instructions
    post = {
        "date": datetime.datetime.utcnow(),
        "instructions" : instructionSet
    }
    collection.insert_one(post)

# Flask

app = Flask(__name__)

@app.route("/instruction")
def instructionPage():
    return render_template('Instruction.html')

@app.route("/postInstructions", methods=['POST'])
def handleInstructions():
    json = request.get_json()
    print(json)
    inserInstructionSet(json)



if (not ON_HEROKU) and __name__ == "__main__":
    app.run(debug=True)
