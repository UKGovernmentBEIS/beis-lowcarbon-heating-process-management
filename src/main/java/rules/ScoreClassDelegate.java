package rules;

import model.AssessScore;
import org.activiti.engine.delegate.DelegateExecution;
import org.activiti.engine.delegate.JavaDelegate;

/**
 * Created by venkatamutyala on 01/11/2017.
 */
public class ScoreClassDelegate implements JavaDelegate {

    public void execute(DelegateExecution execution)  {
        AssessScore score1 = new AssessScore(1);
        AssessScore score2 = new AssessScore(2);
        AssessScore score3 = new AssessScore(3);

        /* First assessment score */
        score1 = getScoreObj(score1, execution);

        /* Second assessment score */
        score2 = getScoreObj(score2, execution);

        /* Third assessment score */
        score3 = getScoreObj(score3, execution);

        printAll(score1); printAll(score2); printAll(score3); //Todo:- remove this line

        /* Bisiness rules logic */
        double deviation = calculateDeviation(score1, score2, score3);

        execution.setVariable("maxDeviation", deviation);
        execution.setVariable("maxDeviation", 5);
    }

    private double calculateDeviation(AssessScore score1, AssessScore score2, AssessScore score3){
        double weightedScore1 = calculateWeightedScore(score1);
        double weightedScore2 = calculateWeightedScore(score2);
        double weightedScore3 = calculateWeightedScore(score3);
        double deviation = Math.max(weightedScore1, Math.max(weightedScore2, weightedScore3))
                         - Math.min(weightedScore1, Math.min(weightedScore2, weightedScore3));

        System.out.println("=== In weightedScores ==" + weightedScore1 +"====="+weightedScore2 +"====="+weightedScore2 );
        System.out.println("===  MAX ==" + Math.max(weightedScore1, Math.max(weightedScore2, weightedScore3)));
        System.out.println("===  MIN ==" + Math.min(weightedScore1, Math.min(weightedScore2, weightedScore3)));
        System.out.println("=== XXXXXXX Deviation XXXXXXX is ==" + deviation);
        return deviation;
    }

    /** Business Rule:-
     *  - Each option represent certain % of final score. eg. Performance contribute 25% of final score
     *    So, adding all parts (which infact is makes 100% of final score) will be 100% of final score
     *  - But the points you selected is upto 10 only [i.e each point of weightage represent 10 times of the actual value.
     *
     * **/
    private double calculateWeightedScore(AssessScore score){

        double performanceenhancement = score.getPerformanceenhancement() * score.getPerformanceenhancementweight() /100.0;
        double performancemanagement = score.getPerformancemanagement() * score.getPerformancemanagementweight()    /100.0;
        double performanceintegration = score.getPerformanceintegration() * score.getPerformanceintegrationweight() /100.0;

        double marketpotential = score.getMarketpotential() * score.getMarketpotentialweight()    /100.0;
        double projectdelivery = score.getProjectdelivery() * score.getProjectdeliveryweight()    /100.0;
        double projectfinancing = score.getProjectfinancing() * score.getProjectfinancingweight() /100.0;
        // Wider objective - Tie Breaker only - Dont add it now
        double widerobj = score.getWiderobj() * score.getWiderobjweight() /100.0;

        System.out.println("=== score " + score.getKey() + "==  " + performanceenhancement);
        System.out.println("=== score " + score.getKey() + "==  " + performancemanagement);
        System.out.println("=== score " + score.getKey() + "==  " + performanceintegration);
        System.out.println("=== score " + score.getKey() + "==  " + marketpotential);
        System.out.println("=== score " + score.getKey() + "==  " + projectdelivery);
        System.out.println("=== score " + score.getKey() + "==  " + projectfinancing);
        System.out.println("=== score " + score.getKey() + "==  " + widerobj);

        double weightedScore = performanceenhancement + performancemanagement + performanceintegration + marketpotential + projectdelivery
                + projectfinancing + widerobj;

        System.out.println("=== weightedScore:- " + weightedScore + ", for 100:-"+ weightedScore*10.0);

        return weightedScore * 10.0;
    }

    private String getStrVariable(DelegateExecution execution, String varName, int key){
        varName = new StringBuffer(varName).append(key).toString();
        return (execution.getVariable(varName) == null) ? "" : execution.getVariable(varName).toString();
    }

    private int getIntVariable(DelegateExecution execution, String varName, int key){
        varName = new StringBuffer(varName).append(key).toString();
        return (execution.getVariable(varName) == null) ? 0 : Integer.parseInt(execution.getVariable(varName).toString());
    }


    private AssessScore getScoreObj(AssessScore score, DelegateExecution execution){
        score.setProjectdesc(getStrVariable(execution, "projectdesc", score.getKey()));
        score.setProjectdesccomment(getStrVariable(execution, "projectdesccomment", score.getKey()));
        score.setProjectdescweight(getIntVariable(execution, "projectdescweight", score.getKey()));

        score.setPerformanceenhancement(getIntVariable(execution, "performanceenhancement", score.getKey()));
        score.setPerformanceenhancementweight(getIntVariable(execution, "performanceenhancementweight", score.getKey()));
        score.setPerformancemanagement(getIntVariable(execution, "performancemanagement", score.getKey()));
        score.setPerformancemanagementweight(getIntVariable(execution, "performancemanagementweight", score.getKey()));
        score.setPerformanceintegration(getIntVariable(execution, "performanceintegration", score.getKey()));
        score.setPerformanceintegrationweight(getIntVariable(execution, "performanceintegrationweight", score.getKey()));
        score.setPerformancecomment(getStrVariable(execution, "performancecomment", score.getKey()));

        score.setMarketpotential(getIntVariable(execution, "marketpotential", score.getKey()));
        score.setMarketpotentialweight(getIntVariable(execution, "marketpotentialweight", score.getKey()));
        score.setMarketpotentialcomment(getStrVariable(execution, "marketpotentialcomment", score.getKey()));

        score.setProjectdelivery(getIntVariable(execution, "projectdelivery", score.getKey()));
        score.setProjectdeliveryweight(getIntVariable(execution, "projectdeliveryweight", score.getKey()));
        score.setProjectdeliverycomment(getStrVariable(execution, "projectdeliverycomment", score.getKey()));

        score.setProjectfinancing(getIntVariable(execution, "projectfinancing", score.getKey()));
        score.setProjectfinancingweight(getIntVariable(execution, "projectfinancingweight", score.getKey()));
        score.setProjectfinancingcomment(getStrVariable(execution, "projectfinancingcomment", score.getKey()));


        score.setWiderobj(getIntVariable(execution, "widerobj", score.getKey()));
        score.setWiderobjweight(getIntVariable(execution, "widerobjweight", score.getKey()));
        score.setWiderobjcomment(getStrVariable(execution, "widerobjcomment", score.getKey()));

        score.setOverallcomment(getStrVariable(execution, "overallcomment", score.getKey()));

        return score;
    }

    private void printAll(AssessScore score){
        System.out.println("--------------------*********" + score.getKey() + "***********-----------------------------");

        System.out.println("=== In Java Service Delegate == projectdesc" + score.getKey() + "       :-" + score.getProjectdesc());
        System.out.println("=== In Java Service Delegate == projectdesccomment" + score.getKey() + ":-" + score.getProjectdesccomment());
        System.out.println("=== In Java Service Delegate == projectdescweight" + score.getKey() + " :-" + score.getProjectdescweight());

        System.out.println("=== In Java Service Delegate == performance" + score.getKey() + "       :-" + score.getPerformanceenhancement());
        System.out.println("=== In Java Service Delegate == performance" + score.getKey() + "       :-" + score.getPerformanceenhancementweight());
        System.out.println("=== In Java Service Delegate == performance" + score.getKey() + "       :-" + score.getPerformancemanagement());
        System.out.println("=== In Java Service Delegate == performance" + score.getKey() + "       :-" + score.getPerformancemanagementweight());
        System.out.println("=== In Java Service Delegate == performance" + score.getKey() + "       :-" + score.getPerformanceintegration());
        System.out.println("=== In Java Service Delegate == performance" + score.getKey() + "       :-" + score.getPerformanceintegrationweight());
        System.out.println("=== In Java Service Delegate == performance" + score.getKey() + "       :-" + score.getPerformancecomment());

        System.out.println("=== In Java Service Delegate == marketpotential" + score.getKey() + "   :-" + score.getMarketpotential());
        System.out.println("=== In Java Service Delegate == marketpotential" + score.getKey() + "   :-" + score.getMarketpotentialcomment());
        System.out.println("=== In Java Service Delegate == marketpotential" + score.getKey() + "   :-" + score.getMarketpotentialweight());

        System.out.println("=== In Java Service Delegate == projectdelivery" + score.getKey() + "   :-" + score.getProjectdelivery());
        System.out.println("=== In Java Service Delegate == projectdelivery" + score.getKey() + "   :-" + score.getProjectdeliverycomment());
        System.out.println("=== In Java Service Delegate == projectdelivery" + score.getKey() + "   :-" + score.getProjectdeliveryweight());

        System.out.println("=== In Java Service Delegate == projectfinancing" + score.getKey() + "  :-" + score.getProjectfinancing());
        System.out.println("=== In Java Service Delegate == projectfinancing" + score.getKey() + "  :-" + score.getProjectfinancingcomment());
        System.out.println("=== In Java Service Delegate == projectfinancing" + score.getKey() + "  :-" + score.getProjectfinancingweight());

        System.out.println("=== In Java Service Delegate == widerobj" + score.getKey() + "          :-" + score.getWiderobj());
        System.out.println("=== In Java Service Delegate == widerobj" + score.getKey() + "          :-" + score.getWiderobjcomment());
        System.out.println("=== In Java Service Delegate == widerobj" + score.getKey() + "          :-" + score.getWiderobjweight());

        System.out.println("=== In Java Service Delegate == overallcomment" + score.getKey() + "    :-" + score.getOverallcomment());

    }
}
