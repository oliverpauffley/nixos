{ config, lib, pkgs, ... }:
# mr is used to manage multiple git repos at the same time
{
  programs.mr = {
    enable = true;
    settings = {
      nixos = {
        checkout =
          "git clone 'https://github.com/oliverpauffley/nixos' 'nixos'";
      };

      "code/work/energy-contracts" = {
        checkout =
          "git clone 'git@github.com:utilitywarehouse/energy-contracts.git' 'energy-contracts'";
      };
      "code/work/ordering-platform-contracts" = {
        checkout =
          "git clone 'git@github.com:utilitywarehouse/ordering-platform-contracts.git' 'ordering-platform-contracts'";
      };
      "code/work/customer-incentives-account-event-store" = {
        checkout =
          "git clone 'git@github.com:utilitywarehouse/customer-incentives-account-event-store.git' 'customer-incentives-account-event-store'";
      };
    };
  };
}

#utilitywarehouse/cx CODEOWNERS
#        * @utilitywarehouse/account-platform @utilitywarehouse/customer-support @utilitywarehouse/org-multi-service-innovation

#utilitywarehouse/customer-incentives-mono CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation @utilitywarehouse/account-platform @utilitywarehouse/org-energy-smart
#
#utilitywarehouse/customer-proposition-referrals CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/quoting-platform CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation @utilitywarehouse/org-partner-acquisition
#
#utilitywarehouse/ordering-platform CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation @utilitywarehouse/org-partner-acquisition
#
#utilitywarehouse/basket-demo CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/postgres-multiple-db-backup-s3 CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-database-restore CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-1999-promo CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-referral-prizedraw CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-bundle-service CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/pao-pkg CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-boost CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-shared CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-contracts CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-incentives-refer-a-friend-landing-page CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-everybody-saves CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/pao-eventstore CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-incentives-sales-signups-bill-imports CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-incentives-tx-log-voider-payloads CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-incentives-referralcode-generator-payloads CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-service-status CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-everybody-saves-app CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-etf CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-incentives-referrer-eligibility-payloads CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/pao-william-graphql CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation
#
#utilitywarehouse/customer-proposition-uswitch CODEOWNERS
#        * @utilitywarehouse/org-multi-service-innovation @utilitywarehouse/org-partner-acquisition
