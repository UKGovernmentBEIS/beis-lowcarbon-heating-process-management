package models

/**
  * Created by venkatamutyala on 12/06/2017.
  */
case class Score(projectdesc:String, projectdesccomment:String, projectdescweight:Int,
                 cost:Int, costcomment:String, costweight:Int,
                 performanceenhancement:Int, performanceenhancementweight:Int,
                 performancemanagement:Int, performancemanagementweight:Int,
                 performanceintegration:Int, performancecomment:String, performanceintegrationweight:Int,
                 marketpotential:Int, marketpotentialcomment:String, marketpotentialweight:Int,
                 projectdelivery:Int, projectdeliverycomment:String, projectdeliveryweight:Int,
                 projectfinancing:Int, projectfinancingcomment:String, projectfinancingweight:Int,
                 widerobjective:Int, widerobjectivecomment:String, widerobjectiveweight:Int,
                 overallcomment:String, weightedscore:Double, tiebreakscore:Double
                )
