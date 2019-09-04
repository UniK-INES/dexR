<?xml version="1.0" encoding="UTF-8" standalone="no"?><og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
    <name>enaviWindPvPricing</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>enaviWindPvPricing</path>
    <decorating>false</decorating>
    <active>true</active>
    <#list windpvpricings as pricing>
    <resource>
		<name>${pricing.name}</name>
		<type>de.iee.enavi.windpvpricing.resource.model.WindPvPricingConfig</type>
		<path>enaviWindPvPricing/${pricing.name}</path>
		<decorating>true</decorating>
		<active>true</active>
	    <resource xsi:type="og:FloatResource">
		<name>minPrice</name>
		<type>org.ogema.core.model.simple.FloatResource</type>
		<path>enaviWindPvPricing/${pricing.name}/minPrice</path>
		<decorating>false</decorating>
		<active>true</active>
		<value>${pricing.minPrice}</value>
	    </resource>
	    <resource xsi:type="og:FloatResource">
		<name>maxPrice</name>
		<type>org.ogema.core.model.simple.FloatResource</type>
		<path>enaviWindPvPricing/${pricing.name}/maxPrice</path>
		<decorating>false</decorating>
		<active>true</active>
		<value>${pricing.maxPrice}</value>
	    </resource>
	    <resource xsi:type="og:FloatResource">
		<name>solarWeight</name>
		<type>org.ogema.core.model.simple.FloatResource</type>
		<path>enaviWindPvPricing/${pricing.name}/solarWeight</path>
		<decorating>false</decorating>
		<active>true</active>
		<value>${pricing.solarWeight}</value>
	    </resource>
	    <resource xsi:type="og:FloatResource">
		<name>cutOffShare</name>
		<type>org.ogema.core.model.simple.FloatResource</type>
		<path>enaviWindPvPricing/${pricing.name}/cutOffShare</path>
		<decorating>false</decorating>
		<active>true</active>
		<value>${pricing.cutOffShare}</value>
	    </resource>
	    <resource xsi:type="og:FloatResource">
		<name>ratedSolarIrradWPerM2</name>
		<type>org.ogema.core.model.simple.FloatResource</type>
		<path>enaviWindPvPricing/${pricing.name}/ratedSolarIrradWPerM2</path>
		<decorating>false</decorating>
		<active>true</active>
		<value>${pricing.ratedSolarIrradWPerM2}</value>
	    </resource>
	    <resource xsi:type="og:FloatResource">
		<name>ratedWindSpeedMPerS</name>
		<type>org.ogema.core.model.simple.FloatResource</type>
		<path>enaviWindPvPricing/${pricing.name}/ratedWindSpeedMPerS</path>
		<decorating>false</decorating>
		<active>true</active>
		<value>${pricing.ratedWindSpeedMPerS}</value>
	    </resource>
	    <resource xsi:type="og:StringResource">
		<name>enaviClientType</name>
		<type>org.ogema.core.model.simple.StringResource</type>
		<path>enaviWindPvPricing/${pricing.name}/enaviClientType</path>
		<decorating>false</decorating>
		<active>true</active>
		<value>${pricing.enaviClientType}</value>
	    </resource>
    </resource>
    </#list>
    <elementType>de.iee.enavi.windpvpricing.resource.model.WindPvPricingConfig</elementType>
</og:resource>
