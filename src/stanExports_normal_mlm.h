// Generated by rstantools.  Do not edit by hand.

/*
    powerms is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    powerms is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with powerms.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.21.0
#include <stan/model/model_header.hpp>
namespace model_normal_mlm_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_normal_mlm");
    reader.add_event(39, 37, "end", "model_normal_mlm");
    return reader;
}
#include <stan_meta_header.hpp>
class model_normal_mlm
  : public stan::model::model_base_crtp<model_normal_mlm> {
private:
        int J;
        vector_d tau_j_hat;
        vector_d se_j;
        double psd_tau;
        double psd_sig_tau;
public:
    model_normal_mlm(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_normal_mlm(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_normal_mlm_namespace::model_normal_mlm";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 5;
            context__.validate_dims("data initialization", "J", "int", context__.to_vec());
            J = int(0);
            vals_i__ = context__.vals_i("J");
            pos__ = 0;
            J = vals_i__[pos__++];
            check_greater_or_equal(function__, "J", J, 0);
            current_statement_begin__ = 6;
            validate_non_negative_index("tau_j_hat", "J", J);
            context__.validate_dims("data initialization", "tau_j_hat", "vector_d", context__.to_vec(J));
            tau_j_hat = Eigen::Matrix<double, Eigen::Dynamic, 1>(J);
            vals_r__ = context__.vals_r("tau_j_hat");
            pos__ = 0;
            size_t tau_j_hat_j_1_max__ = J;
            for (size_t j_1__ = 0; j_1__ < tau_j_hat_j_1_max__; ++j_1__) {
                tau_j_hat(j_1__) = vals_r__[pos__++];
            }
            current_statement_begin__ = 7;
            validate_non_negative_index("se_j", "J", J);
            context__.validate_dims("data initialization", "se_j", "vector_d", context__.to_vec(J));
            se_j = Eigen::Matrix<double, Eigen::Dynamic, 1>(J);
            vals_r__ = context__.vals_r("se_j");
            pos__ = 0;
            size_t se_j_j_1_max__ = J;
            for (size_t j_1__ = 0; j_1__ < se_j_j_1_max__; ++j_1__) {
                se_j(j_1__) = vals_r__[pos__++];
            }
            check_greater_or_equal(function__, "se_j", se_j, 0);
            current_statement_begin__ = 8;
            context__.validate_dims("data initialization", "psd_tau", "double", context__.to_vec());
            psd_tau = double(0);
            vals_r__ = context__.vals_r("psd_tau");
            pos__ = 0;
            psd_tau = vals_r__[pos__++];
            check_greater_or_equal(function__, "psd_tau", psd_tau, 0);
            current_statement_begin__ = 9;
            context__.validate_dims("data initialization", "psd_sig_tau", "double", context__.to_vec());
            psd_sig_tau = double(0);
            vals_r__ = context__.vals_r("psd_sig_tau");
            pos__ = 0;
            psd_sig_tau = vals_r__[pos__++];
            check_greater_or_equal(function__, "psd_sig_tau", psd_sig_tau, 0);
            // initialize transformed data variables
            // execute transformed data statements
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 13;
            num_params_r__ += 1;
            current_statement_begin__ = 14;
            num_params_r__ += 1;
            current_statement_begin__ = 16;
            validate_non_negative_index("eta", "J", J);
            num_params_r__ += J;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_normal_mlm() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        current_statement_begin__ = 13;
        if (!(context__.contains_r("tau")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable tau missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("tau");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "tau", "double", context__.to_vec());
        double tau(0);
        tau = vals_r__[pos__++];
        try {
            writer__.scalar_unconstrain(tau);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable tau: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 14;
        if (!(context__.contains_r("sig_tau")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sig_tau missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sig_tau");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sig_tau", "double", context__.to_vec());
        double sig_tau(0);
        sig_tau = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, sig_tau);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sig_tau: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 16;
        if (!(context__.contains_r("eta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable eta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("eta");
        pos__ = 0U;
        validate_non_negative_index("eta", "J", J);
        context__.validate_dims("parameter initialization", "eta", "vector_d", context__.to_vec(J));
        Eigen::Matrix<double, Eigen::Dynamic, 1> eta(J);
        size_t eta_j_1_max__ = J;
        for (size_t j_1__ = 0; j_1__ < eta_j_1_max__; ++j_1__) {
            eta(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(eta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable eta: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            current_statement_begin__ = 13;
            local_scalar_t__ tau;
            (void) tau;  // dummy to suppress unused var warning
            if (jacobian__)
                tau = in__.scalar_constrain(lp__);
            else
                tau = in__.scalar_constrain();
            current_statement_begin__ = 14;
            local_scalar_t__ sig_tau;
            (void) sig_tau;  // dummy to suppress unused var warning
            if (jacobian__)
                sig_tau = in__.scalar_lb_constrain(0, lp__);
            else
                sig_tau = in__.scalar_lb_constrain(0);
            current_statement_begin__ = 16;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> eta;
            (void) eta;  // dummy to suppress unused var warning
            if (jacobian__)
                eta = in__.vector_constrain(J, lp__);
            else
                eta = in__.vector_constrain(J);
            // transformed parameters
            current_statement_begin__ = 20;
            validate_non_negative_index("tau_j", "J", J);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> tau_j(J);
            stan::math::initialize(tau_j, DUMMY_VAR__);
            stan::math::fill(tau_j, DUMMY_VAR__);
            // transformed parameters block statements
            current_statement_begin__ = 21;
            stan::math::assign(tau_j, add(tau, multiply(sig_tau, eta)));
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            current_statement_begin__ = 20;
            size_t tau_j_j_1_max__ = J;
            for (size_t j_1__ = 0; j_1__ < tau_j_j_1_max__; ++j_1__) {
                if (stan::math::is_uninitialized(tau_j(j_1__))) {
                    std::stringstream msg__;
                    msg__ << "Undefined transformed parameter: tau_j" << "(" << j_1__ << ")";
                    stan::lang::rethrow_located(std::runtime_error(std::string("Error initializing variable tau_j: ") + msg__.str()), current_statement_begin__, prog_reader__());
                }
            }
            // model body
            current_statement_begin__ = 26;
            lp_accum__.add(normal_log<propto__>(tau, 0, psd_tau));
            current_statement_begin__ = 27;
            lp_accum__.add(normal_log<propto__>(sig_tau, 0, psd_sig_tau));
            current_statement_begin__ = 29;
            lp_accum__.add(normal_log<propto__>(eta, 0, 1));
            current_statement_begin__ = 30;
            lp_accum__.add(normal_log<propto__>(tau_j_hat, tau_j, se_j));
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("tau");
        names__.push_back("sig_tau");
        names__.push_back("eta");
        names__.push_back("tau_j");
        names__.push_back("eta_new");
        names__.push_back("y_site_pred");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(J);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(J);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_normal_mlm_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        double tau = in__.scalar_constrain();
        vars__.push_back(tau);
        double sig_tau = in__.scalar_lb_constrain(0);
        vars__.push_back(sig_tau);
        Eigen::Matrix<double, Eigen::Dynamic, 1> eta = in__.vector_constrain(J);
        size_t eta_j_1_max__ = J;
        for (size_t j_1__ = 0; j_1__ < eta_j_1_max__; ++j_1__) {
            vars__.push_back(eta(j_1__));
        }
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            // declare and define transformed parameters
            current_statement_begin__ = 20;
            validate_non_negative_index("tau_j", "J", J);
            Eigen::Matrix<double, Eigen::Dynamic, 1> tau_j(J);
            stan::math::initialize(tau_j, DUMMY_VAR__);
            stan::math::fill(tau_j, DUMMY_VAR__);
            // do transformed parameters statements
            current_statement_begin__ = 21;
            stan::math::assign(tau_j, add(tau, multiply(sig_tau, eta)));
            if (!include_gqs__ && !include_tparams__) return;
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            // write transformed parameters
            if (include_tparams__) {
                size_t tau_j_j_1_max__ = J;
                for (size_t j_1__ = 0; j_1__ < tau_j_j_1_max__; ++j_1__) {
                    vars__.push_back(tau_j(j_1__));
                }
            }
            if (!include_gqs__) return;
            // declare and define generated quantities
            current_statement_begin__ = 35;
            double eta_new;
            (void) eta_new;  // dummy to suppress unused var warning
            stan::math::initialize(eta_new, DUMMY_VAR__);
            stan::math::fill(eta_new, DUMMY_VAR__);
            stan::math::assign(eta_new,normal_rng(0, 1, base_rng__));
            current_statement_begin__ = 36;
            double y_site_pred;
            (void) y_site_pred;  // dummy to suppress unused var warning
            stan::math::initialize(y_site_pred, DUMMY_VAR__);
            stan::math::fill(y_site_pred, DUMMY_VAR__);
            stan::math::assign(y_site_pred,(tau + (sig_tau * eta_new)));
            // validate, write generated quantities
            current_statement_begin__ = 35;
            vars__.push_back(eta_new);
            current_statement_begin__ = 36;
            vars__.push_back(y_site_pred);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    std::string model_name() const {
        return "model_normal_mlm";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "tau";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sig_tau";
        param_names__.push_back(param_name_stream__.str());
        size_t eta_j_1_max__ = J;
        for (size_t j_1__ = 0; j_1__ < eta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "eta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t tau_j_j_1_max__ = J;
            for (size_t j_1__ = 0; j_1__ < tau_j_j_1_max__; ++j_1__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "tau_j" << '.' << j_1__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
        param_name_stream__.str(std::string());
        param_name_stream__ << "eta_new";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "y_site_pred";
        param_names__.push_back(param_name_stream__.str());
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "tau";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sig_tau";
        param_names__.push_back(param_name_stream__.str());
        size_t eta_j_1_max__ = J;
        for (size_t j_1__ = 0; j_1__ < eta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "eta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t tau_j_j_1_max__ = J;
            for (size_t j_1__ = 0; j_1__ < tau_j_j_1_max__; ++j_1__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "tau_j" << '.' << j_1__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
        param_name_stream__.str(std::string());
        param_name_stream__ << "eta_new";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "y_site_pred";
        param_names__.push_back(param_name_stream__.str());
    }
}; // model
}  // namespace
typedef model_normal_mlm_namespace::model_normal_mlm stan_model;
#ifndef USING_R
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
#endif
#endif
