object DaySix {

  def numRedistributionCycles(input: Seq[Int]): (Int, Int) = {
    val knownConfigurations: Seq[Seq[Int]] = Seq(input)

    val (blockConfig, knownConfigs, _) = Stream.iterate((input, knownConfigurations, false)) {
      case (blocksInBankConfig, currentKnownConfig, found) =>
        redistribute(blocksInBankConfig, currentKnownConfig)
    }.dropWhile(!_._3).head

    val cycles = (knownConfigs.length - 1) - knownConfigs.indexOf(blockConfig)

    (knownConfigs.length - 1, cycles)
  }

  def redistribute(input: Seq[Int], knownConfigurations: Seq[Seq[Int]]): (Seq[Int], Seq[Seq[Int]], Boolean) = {
    val blocksFullyAllocated = false
    val redistributionTask = findNextStartingBank(input)
    val emptiedBank = input.updated(redistributionTask.currentBankIndex, 0)
    val initialRedistribution =
      Redistribution(emptiedBank, redistributionTask, blocksFullyAllocated)

    val redistribution = Stream.iterate(initialRedistribution) {
      case Redistribution(blockConfig, currentRedistribution, everythingAllocated) =>
        val allocationResult = allocateBlocks(blockConfig, currentRedistribution)
        val noMoreBlocksLeftToAllocate = allocationResult.redistributionTask.totalBlocks == 0
        Redistribution(allocationResult.allocatedSeq, allocationResult.redistributionTask,
          noMoreBlocksLeftToAllocate)
    }.dropWhile(!_.blocksFullyAllocated).head

    val found = knownConfigurations.contains(redistribution.reAllocatedBank)
    val updatedConfigs = knownConfigurations :+ redistribution.reAllocatedBank
    (redistribution.reAllocatedBank, updatedConfigs, found)
  }

  private def findNextStartingBank(input: Seq[Int]): RedistributionTask = {
    val nextBank = input.zipWithIndex.maxBy(x => x._1)
    RedistributionTask(nextBank._1, nextBank._2)
  }

  private def allocateBlocks(input: Seq[Int], redistributionTask: RedistributionTask): AllocationResult = {
    if (redistributionTask.totalBlocks >= input.length) {
      val remainingToBeDistributed = redistributionTask.totalBlocks - input.length
      AllocationResult(input.map(_ + 1), redistributionTask.copy(totalBlocks = remainingToBeDistributed))
    } else {
      val (last, first) = input.splitAt(redistributionTask.currentBankIndex + 1)
      if (redistributionTask.totalBlocks <= first.length) {
        val allocatedList = allocateUntilEndOfList(first, last, redistributionTask.totalBlocks)
        AllocationResult(allocatedList, redistributionTask.copy(totalBlocks = 0))
      } else {
        val allocatedList = allocateWithListWrapping(first, last, redistributionTask.totalBlocks)
        AllocationResult(allocatedList, redistributionTask.copy(totalBlocks = 0))
      }
    }
  }

  private def allocateUntilEndOfList(first: Seq[Int], last: Seq[Int], blocks: Int): Seq[Int] = {
    val notToBeAllocatedAnyBlocks = first.length - blocks
    last ++ first.take(blocks).map(_ + 1) ++ first.takeRight(notToBeAllocatedAnyBlocks)
  }

  private def allocateWithListWrapping(first: Seq[Int], last: Seq[Int], blocks: Int): Seq[Int] = {
    val remainderForLast = blocks - first.length
    val notToBeAllocatedAnyBlocks = last.length - remainderForLast
    last.take(remainderForLast).map(_ + 1) ++
      last.takeRight(notToBeAllocatedAnyBlocks) ++
      first.map(_ + 1)
  }
}

case class RedistributionTask(totalBlocks: Int, currentBankIndex: Int)

case class AllocationResult(allocatedSeq: Seq[Int], redistributionTask: RedistributionTask)

case class Redistribution(reAllocatedBank: Seq[Int],
                          redistributionTask: RedistributionTask,
                          blocksFullyAllocated: Boolean)
