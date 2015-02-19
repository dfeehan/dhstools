#include <Rcpp.h>
using namespace Rcpp;

#define NO_OVERLAP 0.0
#define NO_EVENT -1.0


//'
//' @title
//' window_intersect
//'
//' @description
//' given two intervals, figure out the intersection between the
//' first and the second
//'
//' NOTE that the intervals are treated as half-open (start,finish]
//' so that an event that happens exactly at time b is counted in
//' (a,b] but not in (b, c]
//'
//' @param a the first window; here, we use all three components
//'              (start, finish, event)
//' @param b the second window; here, we only use the start and finish
//'              time, and ignore any events
//' @return a window containing the exposure and events from a that takes place
//'         during the time given by b; also, if a has an event and it takes
//'         place during b, it is included
//'
//' @details TODO - should write a more detailed description
//'
//'
// [[Rcpp::export]]
NumericVector window_intersect(NumericVector a, NumericVector b)
{
    NumericVector result = NumericVector::create(NO_OVERLAP, NO_OVERLAP, NO_EVENT);

    // if the intervals don't overlap at all, no intersection is possible
    if ((a[1] <= b[0]) || (a[0] >= b[1])) {
        return result;
    }

    result[0] = std::max(a[0], b[0]);
    result[1] = std::min(a[1], b[1]);

    //if (a[2] >= result[0] && a[2] < result[1]) {
    if (a[2] > result[0] && a[2] <= result[1]) {
        result[2] = a[2];
    } else {
        result[2] = NO_EVENT;
    }

    return(result);

}

//'
//' @title
//' cpp_compute_occ_exp
//'
//' @description
//' compute occurrences and exposures from lifelines,
//' age groups, and time intervals
//'
//' @param lambda a matrix of lifelines whose rows are units of observation (individuals),
//'        and whose columns are (in order): start time, end time, event time
//' @param alpha a matrix whose rows are age groups and whose columns are
//'        (in order): start time, end time (both starting from 0)
//' @param alpha_offset a vector with the birthdate of each unit of observation or,
//'        more generally, the offset to use for the age groups in alpha
//' @param tau a matrix of time periods whose rows are units of observation (individuals),
//'        and whose columns are (in order): start time, end time
//' @return a matrix with one row for each unit of observation (individua) whose
//'        columns are (in order): age group 1 exposure, ..., last age group exposure,
//'        age group 1 number of events, ..., last age group number of events
//'
//' @details TODO - should write a more detailed description
//'
// [[Rcpp::export]]
List cpp_compute_occ_exp(NumericMatrix lambda,
                                  NumericMatrix alpha,
                                  NumericVector alpha_offset,
                                  NumericMatrix tau)
{
    int num_obs = lambda.nrow();
    int num_age_groups = alpha.nrow();
    NumericMatrix results_occ = NumericMatrix(num_obs, num_age_groups);
    NumericMatrix results_exp = NumericMatrix(num_obs, num_age_groups);
    NumericVector l_it, l_itj;
    int i, j;

    for(i = 0; i < num_obs;  i++) {

        // get the intersection of the current lifeline with the time period
        l_it = window_intersect(lambda(i,_), tau(i,_));

        for (j = 0; j < num_age_groups; j++) {

            // get the intersection of the current time period lifeline with
            // the current age group
            l_itj = window_intersect(l_it, alpha_offset(i) + alpha(j,_));

            // and store the results in the results matrix
            if (l_itj[0] != NO_OVERLAP) {
                // the difference between the start and the end
                // is the amount of exposure
                results_exp(i, j) = l_itj[1] - l_itj[0];
            } else {
                results_exp(i, j) = 0;
            }

            if (l_itj[2] != NO_EVENT) {
                // only handling single events for now, so anything other
                // than no event is a total of one event
                results_occ(i, j) = 1;
            } else {
                results_occ(i, j) = 0;
            }

        }
    }

    return(List::create(_["occ"] = results_occ,
                        _["exp"] = results_exp));

}

