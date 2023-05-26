<div align="center">

# Math Whiz-ard

</div>

*Math Whiz-ard* is a web app that allows users to practice basic math problems. Intended mainly for kids in elementary or middle school, *Math Whiz-ard* is a web app that can help improve grades and test scores. Unlike Pearson practice tests, our product allows the user to practice as much or as little as they need to feel comfortable with the subject. Users can choose from an array of problem types, including: addition, subtraction, multiplication, division, fractions, algebra, and decimal operations.  Users will be shown the correct answer if they are wrong and once they are done will be shown their score and a short list of problems they got wrong. Lastly, like we have mentioned before, the UI is clean and simple; it is easily understandable and accessible for all, and the best part of the entire package is that there is no paywall!

---

## **Adult Swim**: Team Members
- Logan Park
- Alex Ojemann
- Tim Wilson
- Jon Noranbrock
- Zach Wrubel

# Project Vision Statement
- We aim to create an app that gives elementary and middle school students the means and freedom to practice different concepts from math in varying scales of difficulty.

# Instructions to run
*Math Whiz-ard* can be run locally or on heroku.  To run on Heroku, click our [Link Here](https://mathwhiz-ard.herokuapp.com/).

# Repository Structure
- `Heroku`
    - `db`
        - `init_data`
            - for `SQL` files
    - `heroku`
    - `src`
        - `resources`
            - `css`
            - `img`
            - `js`
                - Each math page gets its own JavaScript page that performs operations like generating and changing problems.
        - `views`
            - Directory of the different pages in our app.
            - `pages`
                - Directory of `ejs` files for each page in our app.
            - `partials`
                - Contains a footer, header, and menu `ejs` file for use in different files in the `pages` directory.
    - `.env`
        - Contains our postgres user, password, and database names.
    - HerokuLink.txt
        - text file with the link to our Heroku app.
    - server.js
- `MilestoneSubmissions`
    - All our milestone documents and a readme
- `Project`
    - `heroku`
        - `.env`
            - contains our Heroku API key.
    - `init_data`
        - for `SQL` files
    - `resources`
        - `css`
        - `img`
        - `js`
            - Each math page gets its own `JavaScript` page that performs operations like generating and changing problems.
    - `views`
        - Directory of the different pages in our app.
- TeamMeetingLog.md
    - A log of some topics covered in early team meetings.

# Application Architecture
- Front-End: CSS, JavaScript, HTML, EJS
- Back-End: JavaScript, Node.js, PostgreSQL, AJAX
- Platform: Web-App
- Development Environment: Heroku

# Project Tracker
We used the agile/scrum methodology to keep track of *Math Whiz-ard* on our [Jira Dashboard](https://csci-3308-summer1-6.atlassian.net/jira/software/projects/A36/boards/1).

# Meeting time with TA
- Thursdays at 3PM - 3:20PM MST (15:00 - 15:20 MST)
- Dwight's Office Hours Link [Here](https://cuboulder.zoom.us/j/95832525120).
<!-- - Dwight's Office Hours Link: https://cuboulder.zoom.us/j/95832525120 -->

# Team Meeting Time
- Thursdays at 7PM - 9PM MST
- [Zoom Link Here](https://cuboulder.zoom.us/j/2172365690)
<!-- - Zoom Link: https://cuboulder.zoom.us/j/2172365690 -->

Note that this is a modified copy of the project. The node_modules folder was too big to upload via the web so some parts of the program may not run if you don't have them installed.
