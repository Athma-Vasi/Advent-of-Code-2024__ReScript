// T(n) = O(n*log(n))
// S(n) = O(1)

let hystorianHysteria = (locationIds1: array<int>, locationIds2: array<int>) => {
  let sortedAsc1 = locationIds1->Array.toSorted((id1, id2) => Int.compare(id1, id2))
  let sortedAsc2 = locationIds2->Array.toSorted((id1, id2) => Int.compare(id1, id2))

  let rec reconcile = (totalDistance: int, index: int) => {
    switch index === Array.length(sortedAsc1) {
    | true => totalDistance
    | false => {
        let smallId1 = switch sortedAsc1->Array.at(index) {
        | None => -1
        | Some(id) => id
        }
        let smallId2 = switch sortedAsc2->Array.at(index) {
        | None => -1
        | Some(id) => id
        }
        let diff = smallId1 - smallId2
        let absDiff = diff < 0 ? diff * -1 : diff

        reconcile(totalDistance + absDiff, index + 1)
      }
    }
  }

  reconcile(0, 0)
}

let l1 = [3, 4, 2, 1, 3, 3]
let l2 = [4, 3, 5, 3, 9, 3]
let r1 = hystorianHysteria(l1, l2)
Console.log2("r1: ", r1) // 11
