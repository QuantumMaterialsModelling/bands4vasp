import sys
import numpy as np
import matplotlib
import os

# Force headless mode if DISPLAY is not available (e.g., on clusters)
if not os.environ.get('DISPLAY'):
    matplotlib.use('Agg')

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Line3DCollection

def read_points(filename):
    """Read x, y, z coordinates from file; ignore further columns (e.g. weights)."""
    points = []
    with open(filename) as f:
        for line in f:
            parts = line.strip().split()
            if len(parts) < 3:
                continue  # Skip empty or malformed lines
            # Only x, y, z are read
            points.append([float(parts[0]), float(parts[1]), float(parts[2])])
    return np.array(points)

def visualize_radial(points):
    """
    Visualize radial sampling: first point is the center, rest are endpoints.
    Output filename includes the number of rays.
    """
    center = points[0]
    ends = points[1:]
    num_rays = len(ends)
    fig = plt.figure(figsize=(8,8))
    ax = fig.add_subplot(111, projection='3d')

    # Draw lines from center to each endpoint
    for ep in ends:
        ax.plot([center[0], ep[0]], [center[1], ep[1]], [center[2], ep[2]], 'b-')
    ax.scatter(center[0], center[1], center[2], color='red', s=60, label='center')
    ax.scatter(ends[:,0], ends[:,1], ends[:,2], color='green', s=40, label='endpoints')

    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('z')
    ax.legend()
    ax.set_title(f'Radial Sampling Visualization\n({num_rays} rays)')
    plt.tight_layout()
    # Compose output filename with path count
    outname = f"radial_sampling_{num_rays}rays.png"
    if matplotlib.get_backend() == 'Agg':
        plt.savefig(outname)
        print(f"Saved image as: {outname}")
    else:
        plt.show()

def visualize_pairs(points):
    """
    Visualize line/surface sampling: every two consecutive points form a path.
    Output filename includes the number of paths.
    """
    fig = plt.figure(figsize=(8,8))
    ax = fig.add_subplot(111, projection='3d')

    n_pairs = len(points) // 2
    # Draw lines between each point pair
    for i in range(n_pairs):
        p1 = points[2*i]
        p2 = points[2*i+1]
        ax.plot([p1[0], p2[0]], [p1[1], p2[1]], [p1[2], p2[2]], 'b-')
        ax.scatter([p1[0], p2[0]], [p1[1], p2[1]], [p1[2], p2[2]], color='green', s=40)
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('z')
    ax.set_title(f'Line/Surface Sampling Visualization\n({n_pairs} paths)')
    plt.tight_layout()
    # Compose output filename with path count
    outname = f"pair_{n_pairs}paths.png"
    if matplotlib.get_backend() == 'Agg':
        plt.savefig(outname)
        print(f"Saved image as: {outname}")
    else:
        plt.show()

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 visualize_kpoints.py <inputfile>")
        sys.exit(1)
    infile = sys.argv[1]
    points = read_points(infile)
    if len(points) < 2:
        print("Not enough points for visualization.")
        sys.exit(1)

    # Decide mode: radial or pairwise
    # Heuristic: radial if file name starts with "radial" OR not an even number of points
    # Otherwise: pair mode (line/surface)
    is_radial = (
        os.path.basename(infile).lower().startswith("radial")
        or (len(points) >= 2 and not np.allclose(points[0], points[1]) and len(points) != 2*((len(points)//2)))
    )

    if is_radial:
        visualize_radial(points)
    else:
        visualize_pairs(points)

if __name__ == "__main__":
    main()
