box to line collisions
calculate how long the normal vector from a line to a point
    get line from two points
        ax + by = c
        (y1 – y2)x + (x2 – x1)y = -(x1y2 – x2y1)
    find perpendicular line that goes through a player's point
        bx - ay for perp line
        fill in x and y from my point to get bx - ay = d
    find intersect
        solve a  b | c
              d  e | f
        x = ce - bf / ae - bd
        y = af - cd / ae - bd
    check if intersect is on the line segment
        use between to get if the point is between the other two points
    check distance between player point and other point
if distances have different signs, collision is present

push back player

