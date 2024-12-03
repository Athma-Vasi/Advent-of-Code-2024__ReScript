// T(n) = O(m * n)
// S(n) = O(1)

type monotonic = Incr | Decr

let redNosedReports = (reports: array<array<int>>) => {
  let checkMonotonicStack = (report: array<int>, prev: int, stack: monotonic) => {
    let rec check = (isStrictlyMonotonic: bool, prev: int, index: int, stack: monotonic) => {
      switch index === Array.length(report) || !isStrictlyMonotonic {
      | true => isStrictlyMonotonic
      | false => {
          let curr = switch report->Array.at(index) {
          | None => -1
          | Some(r) => r
          }
          let diff = prev - curr

          check(
            switch stack {
            | Incr => diff < 0 && diff >= -3
            | Decr => diff > 0 && diff <= 3
            },
            curr,
            index + 1,
            stack,
          )
        }
      }
    }

    check(true, prev, 1, stack)
  }

  let rec countSafeReports = (amount: int, reports: array<array<int>>, reportsIndex: int) => {
    switch reportsIndex === Array.length(reports) {
    | true => amount
    | false => {
        let report = switch reports->Array.at(reportsIndex) {
        | None => []
        | Some(r) => r
        }
        let firstLevel = switch report->Array.at(0) {
        | None => -1
        | Some(l) => l
        }
        let secondLevel = switch report->Array.at(1) {
        | None => -1
        | Some(l) => l
        }

        switch firstLevel > secondLevel {
        | true =>
          countSafeReports(
            checkMonotonicStack(report, firstLevel, Decr) ? amount + 1 : amount,
            reports,
            reportsIndex + 1,
          )

        | false =>
          countSafeReports(
            checkMonotonicStack(report, firstLevel, Incr) ? amount + 1 : amount,
            reports,
            reportsIndex + 1,
          )
        }
      }
    }
  }

  countSafeReports(0, reports, 0)
}

let r11 = [
  [7, 6, 4, 2, 1],
  [1, 2, 7, 8, 9],
  [9, 7, 6, 2, 1],
  [1, 3, 2, 4, 5],
  [8, 6, 4, 4, 1],
  [1, 3, 6, 7, 9],
]
let r1 = redNosedReports(r11)
Console.log2("r1: ", r1)
