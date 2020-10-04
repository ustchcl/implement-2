module Utils where

  bindersOf :: [(a, b)] -> [a]
  bindersOf = map fst
  
  -- right hand sides of
  rhssOf :: [(a, b)] -> [b]
  rhssOf = map snd

  hd :: [a] -> a
  hd = head

  tl :: [a] -> [a]
  tl = tail

  type HeapOperation = (Int, Int, Int)
  -- (lenght, unused address, address objects pair)
  type Heap a = (Int, [Int], [(Int, a)])
  type Addr = Int
  
  contentHeap :: Heap a -> [(Int, a)]
  contentHeap (_, _, arr) = arr

  hInitial :: Heap a
  hInitial = (0, [1..], [])

  hAlloc :: Heap a -> a -> (Heap a, Addr)
  hAlloc (size, x:xs, arr) a = ((size + 1, xs, (x, a):arr), x)

  hUpdate :: Heap a -> Addr -> a -> Heap a
  hUpdate (size, addrs, arr) addr a = (size, addrs, (addr, a) : remove arr addr)

  hFree :: Heap a -> Addr -> Heap a
  hFree (size, addrs, arr) addr = (size - 1, addr:addrs, remove arr addr)
  
  hLookup :: Heap a -> Addr -> a
  hLookup (_, _, arr) addr = aLookup arr addr $ error ("can't find node " ++ showaddr addr ++ " in heap")

  hAddresses :: Heap a -> [Addr]
  hAddresses (_, _, arr) = aDomain arr

  hSize :: Heap a -> Int
  hSize (size, _, _) = size
  
  hNull :: Addr
  hNull = 0

  hIsnull :: Addr -> Bool
  hIsnull addr = addr == 0

  remove :: [(Int, a)] -> Int -> [(Int, a)]
  remove [] a = error $ "Attemp to update or free nonexistent address " ++ showaddr a
  remove ((a', n):cts) a | a == a' = cts
                         | otherwise = (a', n) : remove cts a

  showaddr :: Addr -> String
  showaddr a = "#" ++ show a

  type ASSOC a b = [(a, b)]

  aLookup :: Eq a => [(a, b)] -> a -> b -> b
  aLookup [] a def = def
  aLookup ((a', n) : cts) a  def  | a' == a = n
                                  | otherwise = aLookup cts a def

  aDomain ::ASSOC a b -> [a]
  aDomain = map fst

  aRange ::ASSOC a b -> [b]
  aRange = map snd

  mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
  mapAccuml f a bs = mapAccuml' f bs (a, [])
    where  
      mapAccuml' _ [] acc = acc
      mapAccuml' f (b:bs) (a, cs) = let (a', c) = f a b in mapAccuml' f bs (a', c:cs)

