let rec binary_search arr target low high =
  if low > high then -1
  else
    let mid = (low + high) / 2 in
    if arr.(mid) = target then mid
    else if arr.(mid) > target then
      binary_search arr target low (mid - 1)
    else
      binary_search arr target (mid + 1) high

let find_element arr target =
  binary_search arr target 0 (Array.length arr - 1)
