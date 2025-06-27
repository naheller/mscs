# From Skiena Ch 1.2

## Movie scheduling problem

I = [
        {'title': 'Discrete', 'interval': [2, 4]},
        {'title': 'Process', 'interval': [9, 11.9]},
        {'title': 'President', 'interval': [1, 5]},
        {'title': 'Steiner', 'interval': [5.9, 8]},
        {'title': 'Programming', 'interval': [8.5, 9.7]},
        {'title': 'Tarjan', 'interval': [3, 6]},
        {'title': 'Halting', 'interval': [4.9, 7]},
        {'title': 'Calculated', 'interval': [9.9, 12]},
        {'title': 'Four Volume', 'interval': [7.9, 10]},
        ]

def get_largest(L):
    if len(L) == 0:
        return []
    else:
        L_s = sorted(L, key=lambda movie: movie['interval'][1])
        L_f = [movie for movie in L_s if movie['interval'][0] > L_s[0]['interval'][1]]
        return [L_s[0]] + get_largest(L_f)

X = get_largest(I)
print([x['title'] for x in X])
