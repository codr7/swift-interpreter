from benchmark import benchmark

print(benchmark(10000, '''
def fib(n, lookup):
  if n in lookup: return lookup[n]
  result = n if n < 2 else fib(n-1, lookup) + fib(n-2, lookup)
  lookup[n] = result
  return result
''',
'''fib(70, {})'''))
