from flask import Flask, request, jsonify, send_from_directory
import os

app = Flask(__name__)

@app.route('/')
def home():
    return send_from_directory(os.path.dirname(os.path.abspath(__file__)), 'index.html')

@app.route('/style.css')
def css():
    return send_from_directory(os.path.dirname(os.path.abspath(__file__)), 'style.css')

packages = {
    "Pachetul 1": [
        {"name": "Knowledge-Based Systems", "difficulty": "Medium", "type": "Theoretical", "interest": "AI"},
        {"name": "Programming Techniques on Mobile Platforms", "difficulty": "Hard", "type": "Practical", "interest": "Programming"},
        {"name": "Computational Aspects in the Number Theory", "difficulty": "Hard", "type": "Theoretical", "interest": "Maths"},
        {"name": "Game Design", "difficulty": "Medium", "type": "Mixed", "interest": "Games"},
        {"name": "Advanced Topics in .NET", "difficulty": "Medium", "type": "Practical", "interest": "Programming"}
    ],
    "Pachetul 2": [
        {"name": "Psychology of the Professional Communication in IT industry", "difficulty": "Easy", "type": "Theoretical", "interest": "Communication"},
        {"name": "Cloud Computing", "difficulty": "Hard", "type": "Practical", "interest": "Programming"},
        {"name": "Human-Computer Interaction", "difficulty": "Medium", "type": "Mixed", "interest": "Design"},
        {"name": "Social Media Networks Analysis", "difficulty": "Medium", "type": "Theoretical", "interest": "Data Analysis"}
    ],
    "Pachetul 3": [
        {"name": "Petri Nets and Applications", "difficulty": "Hard", "type": "Theoretical", "interest": "Maths"},
        {"name": "Smart Cards and Applications", "difficulty": "Medium", "type": "Practical", "interest": "Security"},
        {"name": "Automotive Specific Software Engineering", "difficulty": "Hard", "type": "Practical", "interest": "Engineering"},
        {"name": "Introduction to Internet of Things", "difficulty": "Medium", "type": "Mixed", "interest": "Technology"},
        {"name": "Natural Language Processing Techniques", "difficulty": "Medium", "type": "Theoretical", "interest": "AI"}
    ]
}

def calculate_scores(package, difficulty, type_of_course, interest):
    scores = {}
    for option in package:
        score = 0
        if option["difficulty"] == difficulty:
            score += 2
        if option["type"] == type_of_course:
            score += 2
        if option["interest"] == interest:
            score += 3
        scores[option["name"]] = score
    return scores

def select_best_option(scores):
    best_option = max(scores.items(), key=lambda item: item[1])
    return best_option[0]

@app.route('/get_best_options', methods=['POST'])
def get_best_options():
    data = request.get_json()
    difficulty = data.get('difficulty')
    type_of_course = data.get('type')
    interest = data.get('interest')

    results = {}
    for package_name, package in packages.items():
        scores = calculate_scores(package, difficulty, type_of_course, interest)
        best_option = select_best_option(scores)
        results[package_name] = best_option
    
    return jsonify(results)

if __name__ == '__main__':
    app.run(debug=True)
