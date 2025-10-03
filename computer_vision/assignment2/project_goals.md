This assignment builds on your understanding of geometric transformations
and introduces you to real-world applications of feature matching and
homography. You will use concepts from Lecture 6 and beyond, such as
keypoint detection, perspective transformations, and image warping, to solve
a visual puzzle automatically. This is where geometry meets computer vision
magic!
Task 1: Localize a Hidden Object in the Scene [25 Points]
Your goal is to automatically find a hidden object (e.g., an image of Samford
Hall) in a real-world scene using feature matching. Then, using geometric
reasoning, you will highlight where the object appears in the scene.
Instructions:
1. Download the provided images (Part1_scene.png and
auburn.jpeg)
2. Use the following techniques:
a. SIFT keypoint detection and descriptor extraction (via OpenCV)
b. Keypoint matching using Brute-Force or FLANN matcher
c. Lowe’s ratio test to filter noisy matches
d. Homography estimation using RANSAC
e. Perspective transformation to map the template’s bounding box
into the scene
f. Draw the result:
i. Overlay a bounding box (4-point polygon) on the scene
image where the template appears
3. Hint: To estimate the homography matrix H, you need a set of matched
keypoints between the template and scene image, define the 4 corners
of the template image, and project them into the scene.
