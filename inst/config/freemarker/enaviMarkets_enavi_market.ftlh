<?xml version="1.0" encoding="UTF-8" standalone="no"?><og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
    <name>enaviMarkets</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>enaviMarkets</path>
    <decorating>false</decorating>
    <active>true</active>
    <#list markets as market>
    <resource>
        <name>enavi_market</name>
        <type>de.iwes.enavi.util.Marketplace</type>
        <path>enaviMarkets/enavi_market</path>
        <decorating>true</decorating>
        <active>true</active>
        <resource xsi:type="og:StringResource">
            <name>marketURL</name>
            <type>org.ogema.core.model.simple.StringResource</type>
            <path>enaviMarkets/enavi_market/marketURL</path>
            <decorating>false</decorating>
            <active>true</active>
            <value>${market.url}:${market.port}</value>
        </resource>
    </resource>
    </#list>
    <elementType>de.iwes.enavi.util.Marketplace</elementType>
</og:resource>
