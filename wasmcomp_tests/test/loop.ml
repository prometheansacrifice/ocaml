let a b = (
  let result = ref 0 in
  for i = 0 to 1000 do
    result := b * i
  done;
  !result
)
