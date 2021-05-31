<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
	<name>heatDemandSims</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>heatDemandSims</path>
    <decorating>false</decorating>
    <active>true</active>
    <#list hdsims as hdsim>
	<resource>
    
		<name>${hdsim.name}</name>
		<type>de.iee.sim.heatdemand.config.HeatdemandSimConfig</type>
		<path>heatDemandSims/${hdsim.name}</path>
		<decorating>false</decorating>
		<active>true</active>
		<resourcelink>
			<link>enaviSimulatedDevices/${hdsim.building}</link>
			<type>org.ogema.model.locations.Building</type>
			<name>building</name>
		</resourcelink>
		<resourcelink>
			<link>kassel2012Sensors/temperatureSensor</link>
			<type>org.ogema.model.sensors.TemperatureSensor</type>
			<name>outsideTemperature</name>
		</resourcelink>
		<resourcelink>
			<link>enaviSimulatedDevices/${hdsim.building}/temperatureSensor</link>
			<type>org.ogema.model.sensors.TemperatureSensor</type>
			<name>insideTemperature</name>
		</resourcelink>
		<resourcelink>
			<link>enaviSimulatedDevices/${hdsim.building}/energyDemand</link>
			 <type>org.ogema.model.smartgriddata.ThermalEnergyRequest</type>
			<name>energyDemand</name>
		</resourcelink>
		<resourcelink>
			<link>enaviSimulatedDevices/${hdsim.building}/heatConnections/inputStorage/powerSensor</link>
			<type>org.ogema.model.sensors.ThermalPowerSensor</type>
			<name>generation</name>
		</resourcelink>
		<resource xsi:type="og:FloatResource">
			<path>heatDemandSims/${hdsim.name}/targetTemperature</path>
			<type>org.ogema.core.model.units.TemperatureResource</type>
			<name>targetTemperature</name>
			<unit>K</unit>
			<decorating>false</decorating>
			<active>true</active>
			<value>${hdsim.targetTemperature}</value><!-- 20°C -->
		</resource>
		<resource xsi:type="og:FloatResource">
			<path>heatDemandSims/${hdsim.name}/lossFactor</path>
			<type>org.ogema.core.model.units.PowerResource</type>
			<name>lossFactor</name>
			<unit>W</unit>
			<decorating>false</decorating>
			<active>true</active>
			<!-- factor 100 implies e.g. a heat loss of ca. 50kWh at 0°C outside temp and 20°C inside temp -->
			<value>${hdsim.lossFactor}</value>
		</resource>
	</resource>
    </#list>
	<elementType>de.iee.sim.heatdemand.config.HeatdemandSimConfig</elementType>
</og:resource>
