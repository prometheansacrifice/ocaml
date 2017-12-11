let a b = (
  let result = ref 0 in
  for i = 0 to 1000 do
    for j = 0 to 50 do
      result := b * i - j
    done
  done;
  !result
)
