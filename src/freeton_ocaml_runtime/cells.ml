type cell_hash = string

type cell =
  {
    size : int; (* in bits, up to 1023 *)
    exotic : int option; (* None if regular cell, between 0 and 255 otherwise *)
    level : int; (* between 0 and 3, 0 unless exotic cells are involved *)
    data : string; (* up to 128 bytes, padded with 10* *)
    references : cell_hash array; (* up to 4 hashes *)
  }

let serialize { size; exotic; level; data; references; } =
  let dl = String.length data in
  let d1 = (Array.length references) +
           8 * (if Option.is_some exotic then 1 else 0) +
           32 * level
  and d2 = size/8 + (size-1)/8 + 1 in
  let d1 = Char.unsafe_chr d1 and d2 = Char.unsafe_chr d2 in
  let b = Bytes.create (2 + dl + 32 * (Array.length references)) in
  b.[0] <- d1; b.[1] <- d2;
  Bytes.blit_string data 0 b 2 dl;
  let pad = size mod 8 in
  if pad <> 0
  then b.[1+dl] <- Char.unsafe_chr @@ (Char.code @@ Bytes.get b (1+dl)) land (1 lsl pad);
  Array.iteri (fun i hash ->
      Bytes.blit_string hash 0 b (2+dl+i*32) 32
      ) references;
  b

let count_trail i =
  let n = ref 0 in
  let x = ref i in
  if !x land 0b1111 = 0 then (n := !n + 4; x := !x lsr 4);
  if !x land 0b11 = 0 then (n := !n + 2; x := !x lsr 2);
  if !x land 0b1 = 0 then (n := !n + 1);
  !n + 1

let deserialize b =
  if String.length b < 2
  then failwith "Not a cell: no header found";
  let d1 = Char.code b.[0] and d2 = Char.code b.[1] in
  let level = d1/32 in
  let d1 = d1 - level in
  let s = d1 / 8 in
  let d1 = d1 - s in
  let r = d1 in
  if r > 4
  then failwith "Malformed header";
  if s = 1
  then failwith "Exotic cells unsupported"
  else if s <> 0
  then failwith "Malformed header";
  let data_to_pad = d2 mod 2 in
  let data_bytes = d2/2 + data_to_pad in
  let padding =
    if data_to_pad = 0
    then 0
    else count_trail (Char.code @@ b.[1+data_bytes])
  in
  let size = (d2/2) * 8 + padding in
  {
    size;
    exotic = None;
    level;
    data = String.sub b 2 data_bytes;
    references = Array.init r
        (fun i -> String.sub b (2+data_bytes+i*32) 32);
  }

module Builder = struct

  type t =
    {
      mutable size : int;
      exotic : int option;
      level : int;
      data : bytes;
      mutable references : cell_hash list;
      mutable ref_count : int;
    }

  let make () =
    {
      size = 0;
      exotic = None;
      level = 0;
      data = Bytes.create 256;
      references = [];
      ref_count = 0;
    }

  let to_cell { size; exotic; level; data; references; ref_count } =
    let data =
      let decal = size mod 8 in
      if decal = 0
      then Bytes.sub_string data 0 (size/8)
      else begin
        let byte_size = 1 + size/8 in
        let c = Bytes.get data (pred byte_size) in
        let c' = Char.chr @@
          (Char.code c) land
          255 lsl decal lor
          1 lsl (pred decal)
        in
        Bytes.set data (pred byte_size) c';
        Bytes.sub_string data 0 byte_size
      end
    in
    assert (ref_count <= 4);
    let references = references |> List.rev |> Array.of_list in
    ({
      size; exotic; level;
      data; references
    } : cell)

  let store_bits b s offset length =
    if length + b.size > 1024
    then failwith "builder data overflow";
    let length_byte = (length + 7) / 8 in
    let size_byte = b.size / 8 in
    let s =
      let decal = offset mod 8 in
      let offset_byte = offset / 8 in
      if decal = 0
      then
        String.sub s offset_byte length_byte
      else
        String.init length_byte @@
        fun i ->
        Char.chr @@
        Char.code (s.[offset_byte+i]) lsr decal lor
        ( (Char.code s.[succ offset_byte+i]) lsl (8-decal) land 255 )
    in
    let decal = b.size mod 8 in
    if decal = 0
    then Bytes.blit_string s 0 b.data size_byte length_byte
    else String.iteri
        (fun i c ->
           let c = Char.code c in
           Bytes.set b.data (size_byte+i) @@
           Char.chr @@
           ( Char.code (Bytes.get b.data (size_byte+i)) land (255 lsl decal) ) lor
           ( c lsr (8-decal) land 255 );

           Bytes.set b.data (succ size_byte+i) @@
           Char.chr @@ c lsl decal land 255;

        ) s;
    b.size <- b.size + length

  let store_cell b h =
    if b.ref_count >= 4
    then failwith "builder reference overflow"
    else begin
      b.ref_count <- succ b.ref_count;
      b.references <- h :: b.references
    end
end

module Slice = struct
  type t =
    {
      cell : cell;
      mutable offset : int;
      mutable ref_offset : int;
    }

  let of_cell cell =
    { cell; offset = 0; ref_offset = 0; }

  let remaining_bits { cell = { size; _}; offset; ref_offset = _; } =
    size - offset

  let remaining_references
      { cell = { references; _}; offset = _; ref_offset; } =
    (Array.length references) - ref_offset

  let is_at_end slice =
    remaining_bits slice = 0 &&
    remaining_references slice = 0

  let get_bits ({ cell; offset; ref_offset = _; } as slice) length =
    if offset + length > cell.size
    then failwith "data overflow in slice";
    let decal = offset mod 8 in
    let offset_byte = offset / 8 in
    let length_byte = (length + 7) / 8 in
    slice.offset <- offset + length;
    if decal = 0
    then (String.sub cell.data offset length_byte)
    else String.init length_byte @@
      fun i ->
      Char.chr @@
      ( (Char.code cell.data.[offset_byte+i]) lsl (8 - decal) land 255 ) lor
      (Char.code cell.data.[succ offset_byte+i]) lsr decal


  let get_ref ({ cell; offset = _; ref_offset;} as slice) =
    if ref_offset >= Array.length cell.references
    then failwith "reference overflow in slice"
    else begin
      slice.ref_offset <- succ ref_offset;
      cell.references.(ref_offset)
    end

end
