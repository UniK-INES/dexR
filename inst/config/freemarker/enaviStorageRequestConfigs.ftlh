<?xml version="1.0" encoding="UTF-8" standalone="no"?><og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
    <name>storageRequestConfigs</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>storageRequestConfigs</path>
    <decorating>false</decorating>
    <active>false</active>
   <#list requestConfigurations as requestConfig>
    <resource>
        <name>${requestConfig.name}</name>
        <type>de.iwes.enavi.util.DeviceBasedRequestConfiguration</type>
        <path>storageRequestConfigs/${requestConfig.name}</path>
        <decorating>true</decorating>
        <active>true</active>
        <resource>
            <name>priceFluctuation</name>
            <type>org.ogema.model.smartgriddata.Price</type>
            <path>storageRequestConfigs/${requestConfig.name}/priceFluctuation</path>
            <decorating>false</decorating>
            <active>true</active>
            <resource xsi:type="og:FloatResource">
                <name>price</name>
                <type>org.ogema.core.model.simple.FloatResource</type>
                <path>storageRequestConfigs/${requestConfig.name}/priceFluctuation/price</path>
                <decorating>false</decorating>
                <active>true</active>
                <value>${requestConfig.priceFluctuation}</value>
            </resource>
        </resource>
        <resourcelink>
            <link>enaviClients/${requestConfig.client}</link>
            <type>de.iwes.enavi.util.EnaviClient</type>
            <name>client</name>
        </resourcelink>
        <resource>
            <name>averagePrice</name>
            <type>org.ogema.model.smartgriddata.Price</type>
            <path>storageRequestConfigs/${requestConfig.name}/averagePrice</path>
            <decorating>false</decorating>
            <active>true</active>
            <resource xsi:type="og:FloatResource">
                <name>price</name>
                <type>org.ogema.core.model.simple.FloatResource</type>
                <path>storageRequestConfigs/${requestConfig.name}/averagePrice/price</path>
                <decorating>false</decorating>
                <active>true</active>
                <value>${requestConfig.averagePrice}</value>
            </resource>
        </resource>
        <resource>
            <name>averagePriceOffer</name>
            <type>org.ogema.model.smartgriddata.Price</type>
            <path>storageRequestConfigs/${requestConfig.name}/averagePriceOffer</path>
            <decorating>false</decorating>
            <active>true</active>
            <resource xsi:type="og:FloatResource">
                <name>price</name>
                <type>org.ogema.core.model.simple.FloatResource</type>
                <path>storageRequestConfigs/${requestConfig.name}/averagePriceOffer/price</path>
                <decorating>false</decorating>
                <active>true</active>
                <value>${requestConfig.averagePriceOffer}</value>
            </resource>
        </resource>
        <resource xsi:type="og:ResourceList">
            <name>requests</name>
            <type>org.ogema.core.model.ResourceList</type>
            <path>storageRequestConfigs/${requestConfig.name}/requests</path>
            <decorating>true</decorating>
            <active>false</active>
            <elementType>org.ogema.model.smartgriddata.ElectricEnergyRequest</elementType>
        </resource>
        <resourcelink>
            <link>enaviSimulatedDevices/${requestConfig.storage}</link>
            <type>org.ogema.model.devices.storage.ElectricityStorage</type>
            <name>device</name>
        </resourcelink>
        <resource>
            <name>priceOfferFluctuation</name>
            <type>org.ogema.model.smartgriddata.Price</type>
            <path>storageRequestConfigs/${requestConfig.name}/priceOfferFluctuation</path>
            <decorating>false</decorating>
            <active>true</active>
            <resource xsi:type="og:FloatResource">
                <name>price</name>
                <type>org.ogema.core.model.simple.FloatResource</type>
                <path>storageRequestConfigs/${requestConfig.name}/priceOfferFluctuation/price</path>
                <decorating>false</decorating>
                <active>true</active>
                <value>${requestConfig.priceOfferFluctuation}</value>
            </resource>
        </resource>
    </resource>
    </#list>
    <elementType>de.iwes.enavi.util.DeviceBasedRequestConfiguration</elementType>
</og:resource>
