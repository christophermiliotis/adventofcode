import org.scalatest.{FlatSpec, Matchers}

class DayTwoSpec extends FlatSpec with Matchers {

  private val rawInput = "3458\t3471\t163\t1299\t170\t4200\t2425\t167\t3636\t4001\t4162\t115\t2859\t130\t4075\t4269\n2777\t2712\t120\t2569\t2530\t3035\t1818\t32\t491\t872\t113\t92\t2526\t477\t138\t1360\n2316\t35\t168\t174\t1404\t1437\t2631\t1863\t1127\t640\t1745\t171\t2391\t2587\t214\t193\n197\t2013\t551\t1661\t121\t206\t203\t174\t2289\t843\t732\t2117\t360\t1193\t999\t2088\n3925\t3389\t218\t1134\t220\t171\t1972\t348\t3919\t3706\t494\t3577\t3320\t239\t120\t2508\n239\t947\t1029\t2024\t733\t242\t217\t1781\t2904\t2156\t1500\t3100\t497\t2498\t3312\t211\n188\t3806\t3901\t261\t235\t3733\t3747\t3721\t267\t3794\t3814\t3995\t3004\t915\t4062\t3400\n918\t63\t2854\t2799\t178\t176\t1037\t487\t206\t157\t2212\t2539\t2816\t2501\t927\t3147\n186\t194\t307\t672\t208\t351\t243\t180\t619\t749\t590\t745\t671\t707\t334\t224\n1854\t3180\t1345\t3421\t478\t214\t198\t194\t4942\t5564\t2469\t242\t5248\t5786\t5260\t4127\n3780\t2880\t236\t330\t3227\t1252\t3540\t218\t213\t458\t201\t408\t3240\t249\t1968\t2066\n1188\t696\t241\t57\t151\t609\t199\t765\t1078\t976\t1194\t177\t238\t658\t860\t1228\n903\t612\t188\t766\t196\t900\t62\t869\t892\t123\t226\t57\t940\t168\t165\t103\n710\t3784\t83\t2087\t2582\t3941\t97\t1412\t2859\t117\t3880\t411\t102\t3691\t4366\t4104\n3178\t219\t253\t1297\t3661\t1552\t8248\t678\t245\t7042\t260\t581\t7350\t431\t8281\t8117\n837\t80\t95\t281\t652\t822\t1028\t1295\t101\t1140\t88\t452\t85\t444\t649\t1247"

  behavior of "DayTwo"

  it should "calculate the checksum of the spreadsheet" in {
    val input = Seq("5\t1\t9\t5", "7\t5\t3", "2\t4\t6\t8")
    val input2 = Seq("0\t-1\t10\t123", "0\t0\t0", "1")
    DayTwo.calculateChecksum(input) shouldBe 18
    DayTwo.calculateChecksum(input2) shouldBe 124
  }

  it should "calculate the checksum for part two (looking for even division)" in {
    val input = Seq("5\t9\t2\t8", "9\t4\t7\t3", "3\t8\t6\t5")
    DayTwo.calculateChecksumWithDivision(input) shouldBe 9
  }

  it should "give correct answer for the puzzle input" in {
    val puzzleInput = rawInput.split("\n")
    DayTwo.calculateChecksum(puzzleInput) shouldBe 51139
    DayTwo.calculateChecksumWithDivision(puzzleInput) shouldBe 272
  }

}
